/**
 * Copyright 2012-2013 StackMob
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.stackmob.newman
package response

import scalaz._
import effects._
import Scalaz._
import jsonscalaz._
import java.nio.charset.Charset
import java.util.Date
import com.stackmob.newman.Constants._
import net.liftweb.json._
import net.liftweb.json.scalaz.JsonScalaz._
import org.apache.http.HttpHeaders
import com.stackmob.newman.response.HttpResponseCode.HttpResponseCodeEqual
import com.stackmob.newman.serialization.response.HttpResponseSerialization
import com.stackmob.newman.serialization.common.DefaultBodySerialization
import scala.collection.JavaConversions.JConcurrentMapWrapper
import java.util.concurrent.ConcurrentHashMap
import HttpResponse._

case class CachedHttpResponse(override val code: HttpResponseCode,
                              override val headers: Headers,
                              override val rawBody: RawBody,
                              override val timeReceived: Date = new Date) extends HttpResponse {

  private lazy val rawBodyMap = new JConcurrentMapWrapper(new ConcurrentHashMap[Charset, String])
  private lazy val parsedBodyMap = new JConcurrentMapWrapper(new ConcurrentHashMap[(Charset, JSONR[_]), Result[_]])
  private lazy val jValueMap = new JConcurrentMapWrapper(new ConcurrentHashMap[Charset, JValue])
  private lazy val jsonMap = new JConcurrentMapWrapper(new ConcurrentHashMap[(Charset, Boolean), String])

  override def bodyString(implicit charset: Charset = UTF8Charset): String = {
    rawBodyMap.getOrElseUpdate(charset, new String(rawBody, charset))
  }

  override def toJValue(implicit charset: Charset = UTF8Charset): JValue = {
    jValueMap.getOrElseUpdate(charset, toJSON[HttpResponse](this)(getResponseSerialization.writer))
  }

  override def toJson(prettyPrint: Boolean = false)(implicit charset: Charset = UTF8Charset): String = {
    jsonMap.getOrElseUpdate((charset, prettyPrint), {
      if(prettyPrint) {
        pretty(render(toJValue))
      } else {
        compact(render(toJValue))
      }
    })
  }

  override def bodyAs[T](implicit reader: JSONR[T],
                         charset: Charset = UTF8Charset): Result[T] = {
    parsedBodyMap.getOrElseUpdate((charset, reader), {
      validating {
        parse(bodyString(charset))
      } mapFailure { t: Throwable =>
        nel(UncategorizedError(t.getClass.getCanonicalName, t.getMessage, Nil))
      } flatMap { jValue: JValue =>
        fromJSON[T](jValue)
      }
    }).map(_.asInstanceOf[T])
  }

  override def equals(other: Any): Boolean = {
    ~other.cast[HttpResponse].map { resp =>
      StandardHttpResponse(code, headers, rawBody, timeReceived) ==
        StandardHttpResponse(resp.code, resp.headers, resp.rawBody, resp.timeReceived)
    }
  }

}

case class StandardHttpResponse(override val code: HttpResponseCode,
                                override val headers: Headers,
                                override val rawBody: RawBody,
                                override val timeReceived: Date = new Date) extends HttpResponse

trait HttpResponse {

  def code: HttpResponseCode
  def headers: Headers
  def rawBody: RawBody
  def timeReceived: Date

  def bodyString(implicit charset: Charset = UTF8Charset): String = new String(rawBody, charset)

  def toJValue(implicit charset: Charset = UTF8Charset): JValue = toJSON(this)(getResponseSerialization.writer)

  def toJson(prettyPrint: Boolean = false)(implicit charset: Charset = UTF8Charset): String = {
    if(prettyPrint) {
      pretty(render(toJValue))
    } else {
      compact(render(toJValue))
    }
  }

  def bodyAsCaseClass[T <: AnyRef](implicit m: Manifest[T], charset: Charset = UTF8Charset): Result[T] = {
    def theReader(implicit reader: JSONR[T] = DefaultBodySerialization.getReader): JSONR[T] = reader
    fromJSON[T](parse(bodyString(charset)))(theReader)
  }

  def bodyAs[T](implicit reader: JSONR[T],
                charset: Charset = UTF8Charset): Result[T] = validating {
    parse(bodyString(charset))
  } mapFailure { t: Throwable =>
    nel(UncategorizedError(t.getClass.getCanonicalName, t.getMessage, Nil))
  } flatMap { jValue: JValue =>
    fromJSON[T](jValue)
  }

  def bodyAsIfResponseCode[T](expected: HttpResponseCode,
                              decoder: HttpResponse => ThrowableValidation[T]): ThrowableValidation[T] = {
    val valT = for {
      resp <- validationT[IO, Throwable, HttpResponse](validating(this).pure[IO])
      _ <- validationT[IO, Throwable, Unit] {
        if(resp.code === expected) {
          ().success[Throwable].pure[IO]
        } else {
          (UnexpectedResponseCode(expected, resp.code): Throwable).fail[Unit].pure[IO]
        }
      }
      body <- validationT[IO, Throwable, T] {
        io(decoder(resp)).except(t => t.fail[T].pure[IO])
      }
    } yield body
    valT.run.unsafePerformIO
  }

  def bodyAsIfResponseCode[T](expected: HttpResponseCode)
                             (implicit reader: JSONR[T],
                              charset: Charset = UTF8Charset): ThrowableValidation[T] = {
    bodyAsIfResponseCode[T](expected, { resp: HttpResponse =>
      bodyAs[T].mapFailure { errNel: NonEmptyList[Error] =>
        val t: Throwable = JSONParsingError(errNel)
        t
      }
    })
  }

  lazy val eTag: Option[String] = headers.flatMap { headerList: HeaderList =>
    headerList.list.find(h => h._1 === HttpHeaders.ETAG).map(h => h._2)
  }

  lazy val notModified: Boolean = code === HttpResponseCode.NotModified
}

object HttpResponse {

  private[response] def getResponseSerialization(implicit charset: Charset = UTF8Charset) = new HttpResponseSerialization(charset)

  def apply(code: HttpResponseCode, headers: Headers, rawBody: RawBody, timeReceived: Date = new Date): HttpResponse = {
    StandardHttpResponse(code, headers, rawBody, timeReceived)
  }

  def fromJValue(jValue: JValue)(implicit charset: Charset = UTF8Charset): Result[HttpResponse] = {
    fromJSON(jValue)(getResponseSerialization.reader)
  }

  def fromJson(json: String): Result[HttpResponse] = (validating {
    parse(json)
  } mapFailure { t: Throwable =>
    UncategorizedError(t.getClass.getCanonicalName, t.getMessage, List())
  }).liftFailNel.flatMap(fromJValue(_))

  case class UnexpectedResponseCode(expected: HttpResponseCode, actual: HttpResponseCode)
    extends Exception("expected response code %d, got %d".format(expected.code, actual.code))

  case class JSONParsingError(errNel: NonEmptyList[Error]) extends Exception({
    errNel.map { err: Error =>
      err.fold(
        u => "unexpected JSON %s. expected %s".format(u.was.toString, u.expected.getCanonicalName),
        n => "no such field %s in json %s".format(n.name, n.json.toString),
        u => "uncategorized error %s while trying to decode JSON: %s".format(u.key, u.desc)
      )
    }.list.mkString("\n")
  })

  class HttpResponseW(response: HttpResponse) {
    def toCachedResponse: CachedHttpResponse = {
      CachedHttpResponse(response.code, response.headers, response.rawBody, response.timeReceived)
    }
  }

  implicit def httpResponseToCachedHttpResponse(response: HttpResponse): CachedHttpResponse = {
    new HttpResponseW(response).toCachedResponse
  }

}
