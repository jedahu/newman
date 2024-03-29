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

import com.stackmob.newman._
import com.stackmob.newman.dsl._
import org.specs2.Specification
import org.specs2.execute.{Result => SpecsResult}
import java.net.URL
import scalaz._
import Scalaz._

class DSLSpecs extends Specification { def is =
  "DSLSpecs".title                                                                                                      ^
  """
  The Newman DSL is intended to make it easy to construct and execute HTTP requests
  """                                                                                                                   ^
  "GET should"                                                                                                          ^
    "return a HeaderTransformer"                                                                                        ! GetTest().returnsProperFunction ^
                                                                                                                        end ^
  "POST should"                                                                                                         ^
    "return a HeaderAndBodyTransformer"                                                                                 ! PostTest().returnsProperFunction ^
                                                                                                                        end ^
  "PUT should"                                                                                                          ^
    "return a HeaderAndBodyTransformer"                                                                                 ! PutTest().returnsProperFunction ^
                                                                                                                        end ^
  "DELETE should"                                                                                                       ^
    "return a HeaderTransformer"                                                                                        ! DeleteTest().returnsProperFunction ^
                                                                                                                        end ^
  "HEAD should"                                                                                                         ^
    "return a HeaderTransformer"                                                                                        ! HeadTest().returnsProperFunction ^
    "execute a HEAD request"                                                                                            ! HeadTest().executesCorrectly ^
                                                                                                                        end ^
  "HeaderTransformer should"                                                                                            ^
    "correctly add a header"                                                                                            ! HeaderTransformerTest().correctlyAddsAHeader ^
    "correctly add headers"                                                                                             ! HeaderTransformerTest().correctlyAddsHeaders ^
    "correctly prepend headers"                                                                                         ! HeaderTransformerTest().correctlyPrependsHeaders ^
    "correctly set a header"                                                                                            ! HeaderTransformerTest().correctlySetsAHeader ^
    "correctly set headers"                                                                                             ! HeaderTransformerTest().correctlySetsHeaders ^
    "correctly replace headers"                                                                                         ! HeaderTransformerTest().correctlyReplacesHeaders ^
                                                                                                                        end ^
  "HeaderAndBodyTransformer should"                                                                                     ^
    "correctly add a header"                                                                                            ! HeaderAndBodyTransformerTest().correctlyAddsAHeader ^
    "correctly add headers"                                                                                             ! HeaderAndBodyTransformerTest().correctlyAddsHeaders ^
    "correctly set a header"                                                                                            ! HeaderAndBodyTransformerTest().correctlySetsAHeader ^
    "correctly set headers"                                                                                             ! HeaderAndBodyTransformerTest().correctlySetsHeaders ^
    "correctly replace headers"                                                                                         ! HeaderAndBodyTransformerTest().correctlyReplacesHeaders ^
    "correctly prepend headers"                                                                                         ! HeaderAndBodyTransformerTest().correctlyPrependsHeaders ^
    "correctly prepend a body"                                                                                          ! HeaderAndBodyTransformerTest().correctlyPrependsBody ^
    "correctly prepend a body"                                                                                          ! HeaderAndBodyTransformerTest().correctlyPrependsBody ^
    "correctly set a body"                                                                                              ! HeaderAndBodyTransformerTest().correctlySetsBody ^
    "correctly set a body when passed a string"                                                                         ! HeaderAndBodyTransformerTest().correctlySetsStringBody ^
    "correctly replace a body"                                                                                          ! HeaderAndBodyTransformerTest().correctlyReplacesBody ^
                                                                                                                        end

  trait Context extends BaseContext {
    implicit protected val client = new DummyHttpClient
    protected val u: URL = url(http, "stackmob.com").toURL
    protected def ensureEmptyHeaders[T <: Builder](t: T) = {
      t.headers must beEqualTo(Headers.empty)
    }
  }

  case class GetTest() extends Context {
    private val t = GET(u)
    def returnsProperFunction: SpecsResult = {
      (t must beAnInstanceOf[HeaderBuilder]) and ensureEmptyHeaders(t)
    }

    def executesCorrectly: SpecsResult = {
      (t.executeUnsafe must beEqualTo(DummyHttpClient.CannedResponse)) and
      (client.getRequests.size() must beEqualTo(1))
    }
  }

  case class PostTest() extends Context {
    val t = POST(u)
    def returnsProperFunction: SpecsResult = {
      (t must beAnInstanceOf[HeaderAndBodyBuilder]) and
      (ensureEmptyHeaders(t)) and
      (t.body.length must beEqualTo(0))
    }

    def executesCorrectly: SpecsResult = {
      (t.executeUnsafe must beEqualTo(DummyHttpClient.CannedResponse)) and
      (client.postRequests.size() must beEqualTo(1))
    }
  }

  case class PutTest() extends Context {
    private val t = PUT(u)
    def returnsProperFunction: SpecsResult = {
      (t must beAnInstanceOf[HeaderAndBodyBuilder]) and
      (ensureEmptyHeaders(PUT(u))) and
      (t.body.length must beEqualTo(0))
    }

    def executesCorrecty: SpecsResult = {
      (t.executeUnsafe must beEqualTo(DummyHttpClient.CannedResponse)) and
      (client.putRequests.size() must beEqualTo(1))
    }
  }

  case class DeleteTest() extends Context {
    private val t = DELETE(u)
    def returnsProperFunction: SpecsResult = {

      (t must beAnInstanceOf[HeaderBuilder]) and ensureEmptyHeaders(t)
    }

    def executesCorrectly: SpecsResult = {
      (t.executeUnsafe must beEqualTo(DummyHttpClient.CannedResponse)) and
      (client.deleteRequests.size must beEqualTo(1))
    }
  }

  case class HeadTest() extends Context {
    private val t = HEAD(u)
    def returnsProperFunction: SpecsResult = {
      (t must beAnInstanceOf[HeaderBuilder]) and ensureEmptyHeaders(t)
    }

    def executesCorrectly: SpecsResult = {
      (t.executeUnsafe must beEqualTo(DummyHttpClient.CannedResponse)) and
      (client.headRequests.size must beEqualTo(1))
    }
  }

  trait HeaderTransformerTestBase extends Context {
    protected def transformer: Builder

    protected def ensureEqualHeaders(t: Builder, expected: Headers): SpecsResult = (t.headers must haveTheSameHeadersAs(expected))
    protected def ensureEqualHeaders(t: Builder, expected: HeaderList): SpecsResult = ensureEqualHeaders(t, Headers(expected))
    protected def ensureEqualHeaders(t: Builder, expected: Header): SpecsResult = ensureEqualHeaders(t, Headers(expected))


    protected val header1 = "testHeaderName" -> "testHeaderVal"
    protected val header2 = "testHeaderName2" -> "testHeaderVal2"
    protected val headers = nel(header1, header2)
    def correctlyAddsAHeader = ensureEqualHeaders(transformer.addHeaders(header1), header1)
    def correctlyAddsHeaders = ensureEqualHeaders(transformer.addHeaders(headers), headers)
    def correctlyPrependsHeaders: SpecsResult = {
      ensureEqualHeaders(transformer.addHeaders(header1).addHeaders(header2), Headers(header2, header1)) and
      ensureEqualHeaders(transformer.addHeaders(headers).addHeaders(header2), nel(header2, headers.list))
    }
    def correctlySetsAHeader = {
      ensureEqualHeaders(transformer.setHeaders(header1), nel(header1))
    }
    def correctlySetsHeaders = {
      ensureEqualHeaders(transformer.setHeaders(headers), Headers(List(header1, header2)))
    }
    def correctlyReplacesHeaders: SpecsResult = {
      ensureEqualHeaders(transformer.addHeaders(header1).setHeaders(header2), Headers(header2)) and
        ensureEqualHeaders(transformer.addHeaders(headers).setHeaders(header1), nel(header1)) and
        ensureEqualHeaders(transformer.addHeaders(header1).setHeaders(headers), Headers(header1, header2))
    }
  }

  case class HeaderTransformerTest() extends HeaderTransformerTestBase {
    override protected val transformer = GET(u)
  }

  case class HeaderAndBodyTransformerTest() extends HeaderTransformerTestBase {
    override protected val transformer = POST(u)

    def correctlyPrependsBody: SpecsResult = {
      val b1 = "abc".getBytes
      val b2 = "def".getBytes
      val expected = b1 ++ b2
      val resultantBody: Array[Byte] = transformer.addBody(b2).addBody(b1).body
      resultantBody must beEqualTo(expected)
    }

    def correctlySetsBody: SpecsResult = {
      val b1 = "set".getBytes
      val resultantBody: Array[Byte] = transformer.setBody(b1).body
      resultantBody must beEqualTo(b1)
    }

    def correctlySetsStringBody: SpecsResult = {
      val b1 = "set"
      val resultantBody = transformer.setBody(b1).body
      resultantBody must beEqualTo(b1.getBytes(Constants.UTF8Charset))
    }

    def correctlyReplacesBody: SpecsResult = {
      val b1 = "abc".getBytes
      val b2 = "def".getBytes
      val resultantBody: Array[Byte] = transformer.addBody(b1).setBody(b2).body
      resultantBody must beEqualTo(b2)
    }
  }
}
