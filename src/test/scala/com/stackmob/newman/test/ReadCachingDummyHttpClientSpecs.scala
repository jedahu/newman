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

import caching.DummyHttpResponseCacher
import scalacheck._

import org.specs2.{ScalaCheck, Specification}
import com.stackmob.newman.caching.{Milliseconds, HttpResponseCacher}
import com.stackmob.newman.response.HttpResponse
import org.scalacheck._
import Prop._
import java.net.URL
import com.stackmob.newman._
import com.stackmob.newman.request.HttpRequestWithoutBody

class ReadCachingDummyHttpClientSpecs
  extends Specification
  with ScalaCheck
  with ClientVerification
  with CacheVerification { def is =
  "ReadCachingDummyHttpClientSpecs".title                                                                               ^ end ^
  "CachingDummyHttpClient is an HttpClient that caches responses for some defined TTL"                                  ^ end ^
  "get should read from the cache if there is an entry already"                                                         ! getReadsOnlyFromCache ^ end ^
  "get should read through to the cache"                                                                                ! getReadsThroughToCache ^ end ^
  "head should read from the cache if there is a cache entry already"                                                   ! headReadsOnlyFromCache ^ end ^
  "head should read through to the cache"                                                                               ! headReadsThroughToCache ^ end ^
  "POST, PUT, DELETE should not touch the cache"                                                                        ! postPutDeleteIgnoreCache ^ end

  private val genNoHttpResponse: Gen[Option[HttpResponse]] = Gen.value(Option.empty[HttpResponse])
  private val genAlwaysHttpResponse: Gen[Option[HttpResponse]] = for {
    resp <- genHttpResponse
  } yield {
    Some(resp)
  }

  private def genDummyHttpResponseCache(genMbResp: Gen[Option[HttpResponse]]): Gen[DummyHttpResponseCacher] = for {
    mbResp <- genMbResp
    exists <- Gen.oneOf(true, false)
  } yield {
    new DummyHttpResponseCacher(mbResp, (), exists)
  }

  private def genDummyHttpClient: Gen[DummyHttpClient] = for {
    resp <- genHttpResponse
  } yield {
    new DummyHttpClient(() => resp)
  }

  private def createClient(underlying: HttpClient, cache: HttpResponseCacher, millis: Milliseconds) = {
    new ReadCachingHttpClient(underlying, cache, millis)
  }

  private def verifyReadsOnlyFromCache[T <: HttpRequestWithoutBody](fn: (ReadCachingHttpClient, URL, Headers) => T) = {
    forAll(genURL,
      genHeaders,
      genDummyHttpClient,
      genDummyHttpResponseCache(genAlwaysHttpResponse),
      genPositiveMilliseconds) { (url, headers, dummyClient, dummyCache, milliseconds) =>
      val client = createClient(dummyClient, dummyCache, milliseconds)

      val req = fn(client, url, headers)
      val resp = req.executeUnsafe
      //the response should match what's in the cache, not what's in the underlying client
      val respMatches = dummyCache.cannedGet must beSome.like {
        case r => {
          r must beEqualTo(resp)
        }
      }

      respMatches and
      //there should be 1 cache get, a hit
      verifyCacheInteraction(dummyCache, CacheInteraction(1, 0, 0))
      //there should be no client interaction at all, since a cache hit occurred
      verifyClientInteraction(dummyClient, ClientInteraction(0, 0, 0, 0, 0))

    }
  }

  private def verifyReadsThroughToCache[T <: HttpRequestWithoutBody](createRequest: (ReadCachingHttpClient, URL, Headers) => T,
                                                                     createClientInteraction: Int => ClientInteraction) = {
    forAll(genURL,
      genHeaders,
      genDummyHttpClient,
      genDummyHttpResponseCache(genNoHttpResponse)) { (url, headers, dummyClient, dummyCache) =>

      val oneMinute = Milliseconds(6000)

      val client = createClient(dummyClient, dummyCache, oneMinute)
      val req = createRequest(client, url, headers)
      val resp = req.executeUnsafe
      val respVerified = resp must beEqualTo(dummyClient.responseToReturn())
      //there should be a single client call after the cache miss
      val respClientVerified = verifyClientInteraction(dummyClient, createClientInteraction(1))
      //there should be a get call, a miss, then a set call to perform the write back to the cache,
      //after we've talked to the client
      val respCacheVerified = verifyCacheInteraction(dummyCache, CacheInteraction(1, 1, 0))

      respVerified and
      respClientVerified and
      respCacheVerified
    }
  }

  private def getReadsOnlyFromCache = verifyReadsOnlyFromCache { (client, url, headers) =>
    client.get(url, headers)
  }
  private def getReadsThroughToCache = verifyReadsThroughToCache( { (client, url, headers) =>
    client.get(url, headers)
  }, { numGets =>
    ClientInteraction(numGets, 0, 0, 0, 0)
  })

  private def headReadsOnlyFromCache = verifyReadsOnlyFromCache { (client, url, headers) =>
    client.head(url, headers)
  }
  private def headReadsThroughToCache = verifyReadsThroughToCache({ (client, url, headers) =>
    client.head(url, headers)
  }, { numHeads =>
    ClientInteraction(0, 0, 0, 0, numHeads)
  })

  private def postPutDeleteIgnoreCache = forAll(genURL,
    genHeaders,
    genRawBody,
    genDummyHttpClient,
    genDummyHttpResponseCache(genAlwaysHttpResponse),
    genPositiveMilliseconds) { (url, headers, body, dummyClient, dummyCache, milliseconds) =>
    val client = new ReadCachingHttpClient(dummyClient, dummyCache, milliseconds)

    val postRes = client.post(url, headers, body).executeUnsafe must beEqualTo(dummyClient.responseToReturn())
    val putRes = client.put(url, headers, body).executeUnsafe must beEqualTo(dummyClient.responseToReturn())
    val deleteRes = client.delete(url, headers).executeUnsafe must beEqualTo(dummyClient.responseToReturn())

    postRes and
    putRes and
    deleteRes and
    verifyCacheInteraction(dummyCache, CacheInteraction(0, 0, 0)) and
    verifyClientInteraction(dummyClient, ClientInteraction(0, 1, 1, 1, 0))
  }
}

trait CacheVerification { this: Specification =>
  protected case class CacheInteraction(numGets: Int, numSets: Int, numExists: Int)
  protected def verifyCacheInteraction(cache: DummyHttpResponseCacher, interaction: CacheInteraction) = {
    val getCalls = cache.getCalls.size must beEqualTo(interaction.numGets)
    val setCalls = cache.setCalls.size must beEqualTo(interaction.numSets)
    val existsCalls = cache.existsCalls.size must beEqualTo(interaction.numExists)
    getCalls and setCalls and existsCalls
  }
}

trait ClientVerification { this: Specification =>
  protected case class ClientInteraction(numGets: Int, numPosts: Int, numPuts: Int, numDeletes: Int, numHeads: Int)
  protected def verifyClientInteraction(client: DummyHttpClient, interaction: ClientInteraction) = {
    val getReqs = client.getRequests.size must beEqualTo(interaction.numGets)
    val postReqs = client.postRequests.size must beEqualTo(interaction.numPosts)
    val putReqs = client.putRequests.size must beEqualTo(interaction.numPuts)
    val deleteReqs = client.deleteRequests.size must beEqualTo(interaction.numDeletes)
    val headReqs = client.headRequests.size must beEqualTo(interaction.numHeads)
    getReqs and postReqs and putReqs and deleteReqs and headReqs
  }

}
