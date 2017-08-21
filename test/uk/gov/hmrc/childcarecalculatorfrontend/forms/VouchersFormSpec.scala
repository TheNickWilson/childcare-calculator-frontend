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

package uk.gov.hmrc.childcarecalculatorfrontend.forms

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.childcarecalculatorfrontend.FakeCCApplication
import uk.gov.hmrc.childcarecalculatorfrontend.models.YesNoUnsureEnum
import uk.gov.hmrc.play.test.UnitSpec

class VouchersFormSpec extends UnitSpec with FakeCCApplication {

  val testCases = Table(
    ("Has partner", "Error message key"),
    (false, "vouchers.not.selected.error.single"),
    (true, "vouchers.not.selected.error.couple")
  )

  "VouchersForm" when {
    forAll(testCases) { case (hasPartner, errorMessageKey) =>
      s"user has partner = ${hasPartner}" should {

        "accept valid value" when {
          YesNoUnsureEnum.values.foreach { yesNoUnsure => {
            val yesNoUnsureValue = yesNoUnsure.toString
            s"${yesNoUnsureValue} is selected" in {
              val result = new VouchersForm(hasPartner, applicationMessagesApi).form.bind(Map(
                vouchersKey -> yesNoUnsureValue
              ))
              result.hasErrors shouldBe false
              result.value.get.get shouldBe yesNoUnsureValue
            }
          }}
        }

        val invalidValues = List("", "abcd", "1234", "[*]")
        invalidValues.foreach { invalidValue =>
          s"return error (${applicationMessages.messages(errorMessageKey)}) if invalid value '${invalidValue}' is supplied" in {
            val form = new VouchersForm(hasPartner, applicationMessagesApi).form.bind(
              Map(
                vouchersKey -> invalidValue
              )
            )
            form.value shouldBe None
            form.hasErrors shouldBe true
            form.errors.length shouldBe 1
            form.errors.head.message shouldBe applicationMessages.messages(errorMessageKey)
            form.errors.head.message should not be errorMessageKey
          }
        }

      }
    }
  }

}
