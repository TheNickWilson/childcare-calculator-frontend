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

import play.api.i18n.Messages.Implicits._
import play.api.test.Helpers._
import uk.gov.hmrc.childcarecalculatorfrontend.ControllersValidator

class FreeHoursInfoControllerSpec extends ControllersValidator {

  val sut = new FreeHoursInfoController(applicationMessagesApi) {
  }

  validateUrl(freeHoursInfoPath)

  "FreeHoursInfoController" should {
    "load successfully FreeHoursInfo template" when {
      "onPageLoad is called" in {
        val result = await(sut.onPageLoad(request.withSession(validSession)))
        status(result) shouldBe OK
        result.body.contentType.get shouldBe "text/html; charset=utf-8"
      }
    }

    s"redirect successfully to next page (${livingWithPartnerPath})" when {
      "onSubmit is called" in {
        val result = await(sut.onSubmit(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe livingWithPartnerPath
      }
    }
  }

}