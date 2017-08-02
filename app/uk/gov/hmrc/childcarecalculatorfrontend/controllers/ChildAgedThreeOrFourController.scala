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

package uk.gov.hmrc.childcarecalculatorfrontend.controllers

import javax.inject.{Singleton, Inject}
import play.api.Logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Call, AnyContent, Action}
import uk.gov.hmrc.childcarecalculatorfrontend.forms.ChildAgedThreeOrFourForm
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.childcarecalculatorfrontend.views.html.childAgedThreeOrFour
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@Singleton
class ChildAgedThreeOrFourController @Inject()(val messagesApi: MessagesApi) extends I18nSupport with BaseController {

  val keystore: KeystoreService = KeystoreService

  private def getBackUrl()(implicit hc: HeaderCarrier): Future[Call] = {
    keystore.fetchEntryForSession[Boolean](childAgedTwoKey).map { childAgedTwo =>
      if(childAgedTwo.isDefined) {
        routes.ChildAgedTwoController.onPageLoad()
      }
      else {
        routes.LocationController.onPageLoad()
      }
    }
  }

  private def getLocation()(implicit hc: HeaderCarrier): String = {
    Await.result(keystore.fetchEntryForSession[String](locationKey).map(loc => loc.getOrElse("England")),
      Duration(2,"seconds"))
  }

  def onPageLoad: Action[AnyContent] = withSession { implicit request =>
    keystore.fetchEntryForSession[Boolean](childAgedThreeOrFourKey).flatMap { res =>
      getBackUrl.map { backUrl =>
        Ok(
          childAgedThreeOrFour(
            new ChildAgedThreeOrFourForm(messagesApi).form.fill(res),
            backUrl,
            getLocation
          )
        )
      }
    } recover {
      case ex: Exception =>
        Logger.warn(s"Exception from ChildAgedThreeOrFourController.onPageLoad: ${ex.getMessage}")
        Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
    }
  }

  def onSubmit: Action[AnyContent] = withSession { implicit request =>
    new ChildAgedThreeOrFourForm(messagesApi).form.bindFromRequest().fold(
      errors => {
        getBackUrl.map { backUrl =>
          BadRequest(
            childAgedThreeOrFour(
              errors,
              backUrl,
              getLocation
            )
          )
        }.recover {
          case ex: Exception =>
            Logger.warn(s"Exception from ChildAgedThreeOrFourController.onSubmit.getBackUrl: ${ex.getMessage}")
            Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
        }
      },
      success => {
        keystore.cacheEntryForSession[Boolean](childAgedThreeOrFourKey, success.get).map {
          result =>
            Redirect(routes.ExpectChildcareCostsController.onPageLoad())
        } recover {
          case ex: Exception =>
            Logger.warn(s"Exception from ChildAgedThreeOrFourController.onSubmit: ${ex.getMessage}")
            Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
        }
      }
    )
  }
}
