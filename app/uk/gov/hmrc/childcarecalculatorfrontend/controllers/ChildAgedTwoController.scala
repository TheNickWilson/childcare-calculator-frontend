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

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.childcarecalculatorfrontend.forms.ChildAgedTwoForm
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.childcarecalculatorfrontend.utils.CCConstants
import uk.gov.hmrc.childcarecalculatorfrontend.views.html.childAgedTwo
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class ChildAgedTwoController @Inject()(val messagesApi: MessagesApi) extends I18nSupport
  with SessionProvider
  with FrontendController
  with CCConstants {

  val keystore: KeystoreService  = KeystoreService

  def onPageLoad: Action[AnyContent] = withSession { implicit request =>
    keystore.fetchEntryForSession[Boolean](childAgedTwoKey).map { res =>
      Ok(
        childAgedTwo(
          new ChildAgedTwoForm(messagesApi).form.fill(res)
        )
      )
    } recover {
      case e: Exception =>
        Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
    }
  }

  def onSubmit: Action[AnyContent] = withSession { implicit request =>
    new ChildAgedTwoForm(messagesApi).form.bindFromRequest().fold(
      errors => {
        Future(BadRequest(childAgedTwo(errors)))
      },
      success => {
        keystore.cacheEntryForSession(childAgedTwoKey, success.get).map {
          result =>
            // TODO: Go to 3 or 4 years old page
            Redirect(routes.WhatYouNeedController.onPageLoad())
        } recover {
          case e: Exception =>
            Redirect(routes.ChildCareBaseController.onTechnicalDifficulties())
        }
      }
    )
  }
}
