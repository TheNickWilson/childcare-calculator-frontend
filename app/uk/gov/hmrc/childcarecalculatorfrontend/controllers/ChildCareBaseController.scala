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
import scala.concurrent.Future
import uk.gov.hmrc.childcarecalculatorfrontend.views.html._

@Singleton
class ChildCareBaseController @Inject()(val messagesApi: MessagesApi) extends I18nSupport with BaseController {

  val initialController = routes.WhatYouNeedController

  def onPageLoad: Action[AnyContent] = withSession { implicit request =>
    Future.successful {
      Redirect(initialController.onPageLoad)
    }
  }

  def onTechnicalDifficulties: Action[AnyContent] = Action.async { implicit request =>
    Future.successful {
      Ok(
        technicalDifficulties()
      )
    }
  }

  // TODO: Delete when all pages are ready
  def underConstruction(pageName: Option[String] = None): Action[AnyContent] = Action.async { implicit request =>
    Future.successful {
      Ok(
        s"${pageName.getOrElse("This")} page is under construction"
      )
    }
  }

}


