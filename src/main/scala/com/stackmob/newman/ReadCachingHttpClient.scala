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

import scalaz.Scalaz._
import scalaz.effects.IO
import scalaz.concurrent.Promise
import caching._
import request._
import com.stackmob.newman.response.{CachedHttpResponse, HttpResponse}
import java.net.URL

class ReadCachingHttpClient(httpClient: HttpClient,
                            httpResponseCacher: HttpResponseCacher,
                            t: Milliseconds) extends HttpClient {
  import ReadCachingHttpClient._

  override def get(u: URL, h: Headers): GetRequest = new GetRequest with CachingMixin {
    override protected lazy val ttl = t
    override protected val cache = httpResponseCacher
    override protected def doHttpRequest(h: Headers) = {
      httpClient.get(u, h).prepare
    }
    override val url = u
    override val headers = h
  }

  override def post(u: URL, h: Headers, b: RawBody): PostRequest = new PostRequest {
    override def prepareAsync: IO[Promise[HttpResponse]] = httpClient.post(u, h, b).prepareAsync
    override val url = u
    override val headers = h
    override val body = b
  }

  override def put(u: URL, h: Headers, b: RawBody): PutRequest = new PutRequest {
    override def prepareAsync: IO[Promise[HttpResponse]] = httpClient.put(u, h, b).prepareAsync
    override val url = u
    override val headers = h
    override val body = b
  }

  override def delete(u: URL, h: Headers): DeleteRequest = new DeleteRequest {
    override def prepareAsync: IO[Promise[HttpResponse]] = httpClient.delete(u, h).prepareAsync
    override val url = u
    override val headers = h
  }

  override def head(u: URL, h: Headers): HeadRequest = new HeadRequest with CachingMixin {
    override protected lazy val ttl = t
    override protected val cache = httpResponseCacher
    override protected def doHttpRequest(h: Headers) = {
      httpClient.head(u, h).prepare
    }
    override val url = u
    override val headers = h
  }
}

object ReadCachingHttpClient {
  import HttpResponse._
  trait CachingMixin { this: HttpRequest =>
    protected def ttl: Milliseconds
    protected def cache: HttpResponseCacher
    protected def doHttpRequest(headers: Headers): IO[HttpResponse]

    override def prepareAsync: IO[Promise[HttpResponse]] = cache.get(this).flatMap { mbCachedResponse: Option[HttpResponse] =>
      mbCachedResponse some { resp =>
        resp.pure[Promise].pure[IO]
      } none {
        doHttpRequest(headers).flatMap { response: HttpResponse =>
          val cachedResponse: CachedHttpResponse = response
          cache.set(this, cachedResponse, ttl) >| (cachedResponse: HttpResponse).pure[Promise]
        }
      }
    }
  }
}
