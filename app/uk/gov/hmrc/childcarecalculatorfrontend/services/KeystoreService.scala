/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.childcarecalculatorfrontend.services

import play.api.libs.json.{Json, Reads, Format}
import uk.gov.hmrc.childcarecalculatorfrontend.config.CCSessionCache
import uk.gov.hmrc.childcarecalculatorfrontend.utils.CCConstants
import uk.gov.hmrc.http.cache.client.{CacheMap, SessionCache}
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}

object KeystoreService extends KeystoreService {
  val sessionCache: SessionCache = CCSessionCache
}

trait KeystoreService extends CCConstants {
  val sessionCache: SessionCache

  def cacheEntryForSession[T](key: String, data: T)(implicit hc: HeaderCarrier, formats: Format[T]): Future[Option[T]] = {
    sessionCache.cache[T](key, data) map {
      _.getEntry[T](key)
    }
  }

  def fetchEntryForSession[T](key: String)(implicit hc: HeaderCarrier, rds: Reads[T]): Future[Option[T]] = {
    sessionCache.fetchAndGetEntry[T](key)
  }

  def fetch[Household]()(implicit hc: HeaderCarrier, rds: Reads[Household]): Future[Option[Household]] = {
    sessionCache.fetchAndGetEntry[Household](householdKey)
  }

  def cache[Household](data: Household)(implicit hc: HeaderCarrier, formats: Format[Household]): Future[Option[Household]] = {
    sessionCache.cache[Household](householdKey, data) map {
      _.getEntry[Household](householdKey)
    }
  }

  // TODO: Find a better way to do that
  def removeFromSession(key: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    sessionCache.fetch().flatMap { data =>
      if(data.isEmpty || data.get.data.get(key).isEmpty) {
        Future.successful(true)
      }
      else {
        val updatedData = data.get.data.-(key)
        sessionCache.remove().flatMap { res =>
          val savingResult = for ((updatedDataKey, updatedDataValue) <- updatedData) yield {
            sessionCache.cache(updatedDataKey, updatedDataValue)
          }
          def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = {
            f.map(Success(_)).recover({case x => Failure(x)})
          }

          def allAsTrys[T](fItems: Iterable[Future[T]]) = {
            val listOfFutureTrys = fItems.map(futureToFutureTry)
            Future.sequence(listOfFutureTrys)
          }
          allAsTrys(savingResult).map(!_.exists(_.isFailure))
        }
      }
    }
  }
}
