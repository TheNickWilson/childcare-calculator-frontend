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
import uk.gov.hmrc.play.test.UnitSpec

/**
 * Created by user on 05/09/17.
 */
class MaximumEarningsFormSpec extends UnitSpec with FakeCCApplication {

  val testCases = Table(
    ("youPartnerBoth", "Error message key"),
    ("BOTH", "maximum.earning.error.BOTH"),
    ("YOU", "maximum.earning.error.YOU"),
    ("PARTNER", "maximum.earning.error.PARTNER")
  )

  "MaximumEarningsForm" when {
    forAll(testCases) { case (parentPartnerBoth, errorMessageKey) =>
      s"accept for ${parentPartnerBoth}" should {
        "accept value true" in {
          val form = new MaximumEarningsForm(parentPartnerBoth, applicationMessagesApi).form.bind(
            Map(
              maximumEarningsKey -> "true"
            )
          )
          form.value.get.get shouldBe true
          form.hasErrors shouldBe false
          form.errors shouldBe empty
        }

        "accept value false" in {
          val form = new MaximumEarningsForm(parentPartnerBoth, applicationMessagesApi).form.bind(
            Map(
              maximumEarningsKey -> "false"
            )
          )
          form.value.get.get shouldBe false
          form.hasErrors shouldBe false
          form.errors shouldBe empty
        }

        s"return error (${applicationMessages.messages(errorMessageKey)}) if no value supplied" in {
          val form = new MaximumEarningsForm(parentPartnerBoth, applicationMessagesApi).form.bind(
            Map(
              maximumEarningsKey -> ""
            )
          )
          form.value shouldBe None
          form.hasErrors shouldBe true
          form.errors.length shouldBe 1
          form.errors.head.message shouldBe applicationMessages.messages(errorMessageKey)
          form.errors.head.message should not be errorMessageKey
        }

        val invalidValues = List("abcd", "1234")
        invalidValues.foreach { invalidValue =>
          s"return error if invalid value '${invalidValue}' is supplied" in {
            val form = new MaximumEarningsForm(parentPartnerBoth, applicationMessagesApi).form.bind(
              Map(
                maximumEarningsKey -> invalidValue
              )
            )
            form.value shouldBe None
            form.hasErrors shouldBe true
            form.errors.length shouldBe 1
            form.errors.head.message shouldBe "error.boolean"
          }
        }
      }
    }
  }
}
