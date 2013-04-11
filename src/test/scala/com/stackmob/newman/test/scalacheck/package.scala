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

package com.stackmob.newman.test

import org.scalacheck.Gen
import java.util.concurrent.TimeUnit
import com.stackmob.newman._
import com.stackmob.newman.caching._
import com.stackmob.newman.request._
import java.net.URL
import com.stackmob.newman.response.{CachedHttpResponse, HttpResponseCode, HttpResponse}
import HttpResponseCode._

package object scalacheck {
  lazy val genNonEmptyString: Gen[String] = for {
    chars <- Gen.listOf1(Gen.alphaChar)
  } yield {
    val charList = List(chars)
    charList.mkString
  }

  private lazy val timeUnits = Seq[TimeUnit](TimeUnit.DAYS,
    TimeUnit.HOURS,
    TimeUnit.MICROSECONDS,
    TimeUnit.MILLISECONDS,
    TimeUnit.MINUTES,
    TimeUnit.NANOSECONDS,
    TimeUnit.SECONDS)

  lazy val genTimeUnit: Gen[TimeUnit] = Gen.oneOf(timeUnits)

  lazy val genPositiveMilliseconds: Gen[Milliseconds] = for {
    magnitude <- Gen.posNum[Long]
  } yield {
    Milliseconds(magnitude)
  }

  private lazy val httpResponseCodes = Seq[HttpResponseCode](Accepted,
    BadGateway,
    MethodNotAllowed,
    BadRequest,
    ClientTimeout,
    Conflict,
    Created,
    EntityTooLarge,
    FailedDependency,
    Forbidden,
    GatewayTimeout,
    Gone,
    InsufficientStorage,
    InternalServerError,
    LengthRequired,
    Locked,
    MovedPermanently,
    TemporaryRedirect,
    MultipleChoices,
    MultiStatus,
    NoContent,
    NotAcceptable,
    NonAuthoritativeInformation,
    NotFound,
    NotImplemented,
    NotModified,
    Ok,
    PartialContent,
    PaymentRequired,
    PreconditionFailed,
    AuthenticationRequired,
    RequestURITooLarge,
    ResetContent,
    SeeOther,
    Unauthorized,
    ServiceUnavailable,
    UnprocessableEntity,
    UnsupportedMediaType,
    UseProxy,
    HttpVersionNotSupported
  )
  lazy val genHttpResponseCode: Gen[HttpResponseCode] = Gen.oneOf(httpResponseCodes)


  lazy val genHashCode: Gen[HashCode] = for {
    str <- genRawBody
  } yield {
    str.toList
  }

  lazy val genRawBody: Gen[RawBody] = for {
    str <- genNonEmptyString
  } yield {
    str.getBytes("UTF-8")
  }

  lazy val genCachedResponseDelay = for {
    ttl <- genPositiveMilliseconds
    hashCode <- genHashCode
  } yield {
    CachedResponseDelay(ttl, hashCode)
  }

  lazy val genHeader: Gen[Header] = for {
    key <- genNonEmptyString
    value <- genNonEmptyString
  } yield {
    key -> value
  }

  lazy val genHeaders: Gen[Headers] = for {
    headers <- Gen.listOf(genHeader)
  } yield {
    Headers(headers)
  }

  lazy val genURL: Gen[URL] = for {
    urlString <- genNonEmptyString
  } yield {
    new URL("http://%s.com".format(urlString))
  }

  def genHttpRequest(client: HttpClient): Gen[HttpRequest] = for {
    url <- genURL
    headers <- genHeaders
  } yield {
    client.get(url, headers)
  }

  lazy val genHttpResponse: Gen[HttpResponse] = for {
    code <- genHttpResponseCode
    headers <- genHeaders
    body <- genRawBody
  } yield {
    HttpResponse(code, headers, body)
  }

  lazy val genCachedHttpResponse: Gen[HttpResponse] = for {
    code <- genHttpResponseCode
    headers <- genHeaders
    body <- genRawBody
  } yield {
    CachedHttpResponse(code, headers, body)
  }

}
