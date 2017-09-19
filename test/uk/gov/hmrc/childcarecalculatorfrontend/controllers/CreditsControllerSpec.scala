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

import org.jsoup.Jsoup
import org.mockito.Matchers.any
import org.mockito.Mockito.{reset, when}
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.{Format, Reads}
import play.api.test.Helpers._
import uk.gov.hmrc.childcarecalculatorfrontend.ControllersValidator
import uk.gov.hmrc.childcarecalculatorfrontend.models.AgeRangeEnum.AgeRangeEnum
import uk.gov.hmrc.childcarecalculatorfrontend.models.YouPartnerBothEnum.YouPartnerBothEnum
import uk.gov.hmrc.childcarecalculatorfrontend.models._
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class CreditsControllerSpec extends ControllersValidator with BeforeAndAfterEach {

  val sut = new CreditsController(applicationMessagesApi) {
    override val keystore: KeystoreService = mock[KeystoreService]
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(sut.keystore)
  }

  validateUrl(creditsPath)

  def buildPageObjects(isPartner: Boolean,
                       parentAgeRange: Option[AgeRangeEnum] = None,
                       partnerAgeRange: Option[AgeRangeEnum] = None,
                       parentEarnMoreThanNMW: Option[Boolean] = None,
                       partnerEarnMoreThanNMW: Option[Boolean] = None,
                       whichOfYouInPaidEmployment: Option[YouPartnerBothEnum] = None
                      ): PageObjects = {
    val parent = Claimant(ageRange = parentAgeRange, minimumEarnings = Some(MinimumEarnings(earnMoreThanNMW = parentEarnMoreThanNMW)))
    val partner = Claimant(ageRange = partnerAgeRange, minimumEarnings = Some(MinimumEarnings(earnMoreThanNMW = partnerEarnMoreThanNMW)))

    if (isPartner) {
      PageObjects(whichOfYouInPaidEmployment = whichOfYouInPaidEmployment, household = Household(location = LocationEnum.ENGLAND, parent = parent,
        partner = Some(partner)))
    } else {
      PageObjects(whichOfYouInPaidEmployment = whichOfYouInPaidEmployment, household = Household(location = LocationEnum.ENGLAND, parent = parent))
    }
  }

  "CreditsController" when {

    "onPageLoad is called" should {

      "redirect to error page if can't connect with keystore" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.failed(new RuntimeException)
        )
        val result = await(sut.onPageLoad()(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "redirect to technical difficulties page if there is no data in keystore" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            None
          )
        )
        val result = await(sut.onPageLoad()(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "load template when user visiting the page first time" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = false, parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR), parentEarnMoreThanNMW = None))
          )
        )
        val result = await(sut.onPageLoad()(request.withSession(validSession)))
        status(result) shouldBe OK
      }

      "load template successfully if there is data in keystore and define correct backURL" when {

        "redirect to maximum earnings page when single parent satisfy minimum earnings" in {
        }

        "redirect to self employed/apprentice page when single parent not satisfy minimum earnings and apprentice or neither is selected on " +
          "self employed/apprentice page" in {
        }

        "redirect to self employed page when single parent not satisfy minimum earnings & select self employed on self employed/apprentice page" in {
        }

        "redirect to parent maximum earnings page when only parent is in paid employment & satisfy minimum earnings" in {
        }

        "redirect to parent self employed/apprentice page when only parent in paid employment & not satisfy minimum earnings & apprentice or " +
          "neither is selected on self employed/apprentice page" in {
        }

        "redirect to parent self employed page when only parent in paid employment & not satisfy minimum earnings & select self employed on self " +
          "employed/ apprentice page" in {
        }

        "redirect to partner maximum earnings page when only partner is in paid employment & satisfy minimum earnings" in {
        }

        "redirect to partner self employed/apprentice page when only partner in paid employment & not satisfy minimum earnings & apprentice or " +
          "neither is selected on self employed/apprentice page" in {
        }

        "redirect to partner self employed page when only parent in paid employment & not satisfy minimum earnings & select self employed on self " +
          "employed/ apprentice page" in {
        }

        "redirect to both maximum earnings page when both are in paid employment & satisfy minimum earnings" in {
        }

        "redirect to partner maximum earnings page when both are in paid employment & partner satisfy & parent not satisfy min earnings" in {
        }

        "redirect to parent maximum earnings page when both are in paid employment & parent satisfy & partner not satisfy min earnings" in {
        }

        "redirect to partner self employed/apprentice page when both are in paid employment & not satisfy minimum earnings & apprentice or " +
          "neither is selected on partner self employed/apprentice page" in {
        }

        "redirect to partner self employed page when both are in paid employment & not satisfy minimum earnings & selects self employed on partner " +
          "self employed/ apprentice page" in {
        }

      }

    }

    "onSubmit is called" should {

      s"redirect to ${technicalDifficultiesPath} when unable to store in keystore" in {
        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(true, None))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.failed(new RuntimeException)
        )

        val result = await(
          sut.onSubmit()(
            request
              .withFormUrlEncodedBody(creditsKey -> CreditsEnum.UNIVERSALCREDIT.toString)
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "load template when there is no input, return BAD_REQUEST" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(true, parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR), partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR)))
          )
        )
        val result = await(
          sut.onSubmit()(
            request
              .withFormUrlEncodedBody(creditsKey -> "")
              .withSession(validSession)
          )
        )
        status(result) shouldBe BAD_REQUEST
        result.body.contentType.get shouldBe "text/html; charset=utf-8"
      }

      "redirect to technical difficulties page when there is no data in keystore" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            None
          )
        )

        val result = await(
          sut.onSubmit()(
            request
              .withFormUrlEncodedBody(creditsKey -> "123")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      s"successful submission with location=England, has child of 3 or 4 years, satisfy minimum earnings and earnings less than £100,000, " +
        "redirect to maximum free hours info page" in {
      }

      s"successful submission with location=England, no child of 3 or 4 years, satisfy minimum earnings and earnings less than £100,000, " +
        "redirect to how many children page" in {
      }

      s"successful submission with location=England, no child of 3 or 4 years, satisfy minimum earnings and earnings greater than £100,000, " +
        "redirect to how many children page" in {
      }

      s"successful submission with location=England, no child of 3 or 4 years, not satisfy minimum earnings and not self employed or neither, " +
        "redirect to how many children page" in {
      }

      s"successful submission with location=England, has child of 3 or 4 years, not satisfy minimum earnings and is apprentice, " +
        "redirect to maximum free hours info page" in {
      }

      s"successful submission with location=England, has only child of 3 or 4 years, satisfy minimum earnings and earnings greater than £100,000, " +
        "redirect to result page" in {
      }

    }
  }

}