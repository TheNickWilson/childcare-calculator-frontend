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

import javax.inject.{Inject, Singleton}

import play.api.Logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, Result}
import uk.gov.hmrc.childcarecalculatorfrontend.forms.CreditsForm
import uk.gov.hmrc.childcarecalculatorfrontend.models.YouPartnerBothEnum.YouPartnerBothEnum
import uk.gov.hmrc.childcarecalculatorfrontend.models.{CreditsEnum, EmploymentStatusEnum, PageObjects, YouPartnerBothEnum}
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.childcarecalculatorfrontend.utils.HelperManager
import uk.gov.hmrc.childcarecalculatorfrontend.views.html.credits
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

@Singleton
class CreditsController @Inject()(val messagesApi: MessagesApi) extends I18nSupport with BaseController {

  val keystore: KeystoreService = KeystoreService

  def onPageLoad: Action[AnyContent] = withSession { implicit request =>
    keystore.fetch[PageObjects]().map {
      case Some(pageObjects) =>
        val creditsValue = pageObjects.household.credits.getOrElse("").toString
        Ok(credits(
          new CreditsForm(messagesApi).form.fill(Some(creditsValue)), getBackUrl(pageObjects)
        ))
      case _ =>
        Logger.warn("Invalid PageObjects in CreditsController.onPageLoad")
        Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
    }.recover {
      case ex: Exception =>
        Logger.warn(s"Exception from CreditsController.onPageLoad: ${ex.getMessage}")
        Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
    }
  }

  def onSubmit: Action[AnyContent] = withSession { implicit request =>
    keystore.fetch[PageObjects]().flatMap {
      case Some(pageObjects) =>
        new CreditsForm(messagesApi).form.bindFromRequest().fold(
          errors => {
            Future(BadRequest(credits(errors, getBackUrl(pageObjects))))
          },
          success => {
            nextPage(pageObjects, success.get)
          }
        )
      case _ =>
        Logger.warn("PageObjects object is missing in CreditsController.onSubmit")
        Future(Redirect(routes.ChildCareBaseController.onTechnicalDifficulties()))
    } recover {
      case ex: Exception =>
        Logger.warn(s"Exception from CreditsController.onSubmit: ${ex.getMessage}")
        Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
    }

  }

  private def getBackUrl(pageObjects: PageObjects): Call = {
    val paidEmployment: YouPartnerBothEnum = HelperManager.defineInPaidEmployment(pageObjects)
    paidEmployment match {
      case YouPartnerBothEnum.YOU => {
        if (pageObjects.household.parent.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity)) {
          routes.MaximumEarningsController.onPageLoad(YouPartnerBothEnum.YOU.toString)
        } else if (pageObjects.household.parent.minimumEarnings.get.employmentStatus.contains(EmploymentStatusEnum.SELFEMPLOYED)) {
          routes.SelfEmployedController.onPageLoad(false)
        } else {
          routes.SelfEmployedOrApprenticeController.onPageLoad(false)
        }
      }
      case YouPartnerBothEnum.PARTNER => {
        if (pageObjects.household.partner.get.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity)) {
          routes.MaximumEarningsController.onPageLoad(YouPartnerBothEnum.PARTNER.toString)
        } else if (pageObjects.household.partner.get.minimumEarnings.get.employmentStatus.contains(EmploymentStatusEnum.SELFEMPLOYED)) {
          routes.SelfEmployedController.onPageLoad(true)
        } else {
          routes.SelfEmployedOrApprenticeController.onPageLoad(true)
        }
      }
      case YouPartnerBothEnum.BOTH => {
        if (pageObjects.household.partner.get.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity) &&
          pageObjects.household.parent.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity)) {
          routes.MaximumEarningsController.onPageLoad(YouPartnerBothEnum.BOTH.toString)
        } else if(pageObjects.household.parent.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity) &&
          !pageObjects.household.partner.get.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity)) {
          routes.MaximumEarningsController.onPageLoad(YouPartnerBothEnum.YOU.toString)
        } else if(pageObjects.household.partner.get.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity) &&
          !pageObjects.household.parent.minimumEarnings.get.earnMoreThanNMW.fold(false)(identity)) {
          routes.MaximumEarningsController.onPageLoad(YouPartnerBothEnum.PARTNER.toString)
        } else if (pageObjects.household.partner.get.minimumEarnings.get.employmentStatus.contains(EmploymentStatusEnum.SELFEMPLOYED)) {
          routes.SelfEmployedController.onPageLoad(true)
        } else {
          routes.SelfEmployedOrApprenticeController.onPageLoad(true)
        }
      }
    }
  }

  private def nextPage(pageObjects: PageObjects, selectedCredits: String)(implicit hc: HeaderCarrier): Future[Result] = {
    val modifiedPageObjects: PageObjects = getModifiedPageObjects(pageObjects, selectedCredits)

    keystore.cache(modifiedPageObjects).map { res =>
      if (selectedCredits == CreditsEnum.TAXCREDITS.toString) {
        Redirect(routes.ChildCareBaseController.underConstruction())
      } else if (selectedCredits == CreditsEnum.UNIVERSALCREDIT.toString) {
        Redirect(routes.ChildCareBaseController.underConstruction())
      } else {
        Redirect(routes.ChildCareBaseController.underConstruction())
      }
    }
  }

  private def getModifiedPageObjects(pageObjects: PageObjects, selectedCredits: String): PageObjects = {
    pageObjects.copy(
      household = pageObjects.household.copy(credits = Some(CreditsEnum.withName(selectedCredits)))
    )
  }

}