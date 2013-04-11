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

import net.liftweb.json.scalaz.JsonScalaz._

package object jsonscalaz {
  trait ErrorW {
    protected def error: Error

    def fold[T](unexpected: UnexpectedJSONError => T,
                noSuchField: NoSuchFieldError => T,
                uncategorized: UncategorizedError => T): T = error match {
      case u@UnexpectedJSONError(_, _) => unexpected(u)
      case n@NoSuchFieldError(_, _) => noSuchField(n)
      case u@UncategorizedError(_, _, _) => uncategorized(u)
    }
  }
  implicit def errorToW(e: Error): ErrorW = new ErrorW {
    override lazy val error = e
  }

  sealed trait JSONRW[T] {
    protected def jsonR: JSONR[T]
    def caching = CachingJSONR[T](jsonR)
  }
  implicit def jsonRToW[T](r: JSONR[T]): JSONRW[T] = new JSONRW[T] {
    override lazy val jsonR = r
  }


}
