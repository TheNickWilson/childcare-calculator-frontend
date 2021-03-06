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
import uk.gov.hmrc.childcarecalculatorfrontend.models.YouPartnerBothEnum._
import uk.gov.hmrc.childcarecalculatorfrontend.models._
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class MinimumEarningsControllerSpec extends ControllersValidator with BeforeAndAfterEach {

  val sut = new MinimumEarningsController(applicationMessagesApi) {
    override val keystore: KeystoreService = mock[KeystoreService]
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(sut.keystore)
  }

  validateUrl(minimumEarningsParentPath)
  validateUrl(minimumEarningsPartnerPath)

  "MinimumEarningsController" when {

    "onPageLoad is called" should {
      "redirect to technical difficulty page if there is no data in keystore for parent" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            None
          )
        )
        val result = await(sut.onPageLoad(false)(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(technicalDifficultiesPath))
      }

      "redirect to technical difficulty page if there is no data in keystore for partner" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            None
          )
        )
        val result = await(sut.onPageLoad(true)(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(technicalDifficultiesPath))
      }

      "load template when user visiting the page first time for partner" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true, parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR), parentEarnMoreThanNMW = None,
              partnerAgeRange = Some(AgeRangeEnum.UNDER18), partnerEarnMoreThanNMW = None
            ))
          )
        )
        val result = await(sut.onPageLoad(true)(request.withSession(validSession)))
        status(result) shouldBe OK
      }

      "load template when user visiting the page first time for parent" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = false, parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR), parentEarnMoreThanNMW = None))
          )
        )
        val result = await(sut.onPageLoad(false)(request.withSession(validSession)))
        status(result) shouldBe OK
      }

      "load template successfully if there is data in keystore for parent and define correctly backURL" when {
        "redirect to parent's age page" in {
          when(
            sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
          ).thenReturn(
            Future.successful(
              Some(buildPageObjects(isPartner = false, parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR), parentEarnMoreThanNMW = Some(true)))
            )
          )
          val result = await(sut.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe whatsYourAgePath + "/parent"
        }

        "redirect to partner's age page when both are in paid employment" in {
          when(
            sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
          ).thenReturn(
            Future.successful(
              Some(buildPageObjects(isPartner = false, parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR), parentEarnMoreThanNMW = Some(true),
                whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH)))
            )
          )
          val result = await(sut.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe whatsYourAgePath + "/partner"
        }

        "redirect to error page if can't connect with keystore if parent" in {
          when(
            sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
          ).thenReturn(
            Future.failed(new RuntimeException)
          )
          val result = await(sut.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) should be(Some(technicalDifficultiesPath))
        }
      }

      "load template successfully if there is data in keystore for partner and display correct backurl" when {
        "redirect to partner's age page" in {
          when(
            sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
          ).thenReturn(
            Future.successful(
              Some(buildPageObjects(isPartner = true,
                parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
                partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
                parentEarnMoreThanNMW = Some(true),
                partnerEarnMoreThanNMW = Some(true)))
            )
          )
          val result = await(sut.onPageLoad(true)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe whatsYourAgePath + "/partner"
        }

        "redirect to parent's minimum earnings page if both are in paid employment" in {
          when(
            sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
          ).thenReturn(
            Future.successful(
              Some(buildPageObjects(isPartner = true,
                parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
                partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
                parentEarnMoreThanNMW = Some(true),
                whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
                partnerEarnMoreThanNMW = Some(true)))
            )
          )
          val result = await(sut.onPageLoad(true)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe minimumEarningsParentPath
        }

        "redirect to error page if can't connect with keystore if partner" in {
          when(
            sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
          ).thenReturn(
            Future.failed(new RuntimeException)
          )
          val result = await(sut.onPageLoad(true)(request.withSession(validSession)))
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) should be(Some(technicalDifficultiesPath))
        }
      }

      "connecting with keystore fails" should {
        s"redirect to ${technicalDifficultiesPath}" in {
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
            sut.onSubmit(true)(
              request
                .withFormUrlEncodedBody(whatsYourAgeKey -> AgeRangeEnum.OVERTWENTYFOUR.toString)
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) should be(Some(technicalDifficultiesPath))
        }
      }
    }
  }

  "onSubmit is called" when {

    "there are errors" should {
      "load same template and return BAD_REQUEST as a partner" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR)))
          )
        )
        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "")
              .withSession(validSession)
          )
        )
        status(result) shouldBe BAD_REQUEST
        result.body.contentType.get shouldBe "text/html; charset=utf-8"
      }

      "load same template and return BAD_REQUEST as a parent" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(false,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR)))
          )
        )
        val result = await(
          sut.onSubmit(false)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "")
              .withSession(validSession)
          )
        )
        status(result) shouldBe BAD_REQUEST
        result.body.contentType.get shouldBe "text/html; charset=utf-8"
      }

      "has been previously selected and there is no data in keystore for PageObjects object for partner" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            None
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "123")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(technicalDifficultiesPath))
      }

    }

    "saving in keystore is successful as a partner" should {

      s"both satisfy minimum earnings, redirect to ${maximumEarningsPath}" in {
        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = Some(true)  ,
              partnerEarnMoreThanNMW = Some(true),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH)))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = Some(true),
              partnerEarnMoreThanNMW = Some(true)))
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "true")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(maximumEarningsPath))
      }

      s"both satisfy minimum earnings, parent has none for earnings more than NMWredirect to ${selfEmployedOrApprenticeParentPath}" in {
        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = None  ,
              partnerEarnMoreThanNMW = Some(true),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH)))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = None,
              partnerEarnMoreThanNMW = Some(true)))
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "true")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(selfEmployedOrApprenticeParentPath))
      }

      s"parent not satisfy minimum earnings, redirect to ${selfEmployedOrApprenticeParentPath}" in {
        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = Some(false),
              partnerEarnMoreThanNMW = Some(true),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH)))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = Some(false),
              partnerEarnMoreThanNMW = Some(true)))
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "true")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(selfEmployedOrApprenticeParentPath))
      }

      s"partner not satisfy, parent satisfy minimum earnings, redirect to ${selfEmployedOrApprenticePartnerPath}" in {

        val modelToFetch = buildPageObjects(isPartner = true,
          parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
          partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
          parentEarnMoreThanNMW = Some(true),
          partnerEarnMoreThanNMW = Some(false),
          whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH))

        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(modelToFetch.copy(household = modelToFetch.household.copy(
              partner = Some(modelToFetch.household.partner.fold(Claimant())(_.copy(minimumEarnings = None))))))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = Some(true),
              partnerEarnMoreThanNMW = Some(false),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH)))
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "false")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(selfEmployedOrApprenticePartnerPath))
      }

      s"both not satisfy minimum earnings, redirect to ${selfEmployedOrApprenticeParentPath}" in {
        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
              parentEarnMoreThanNMW = Some(false),
              partnerEarnMoreThanNMW = None))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = Some(false),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
              partnerEarnMoreThanNMW = Some(false)))
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "false")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(selfEmployedOrApprenticeParentPath))
      }

      s"only partner satisfy minimum earnings, redirect to ${maximumEarningsPartnerPath}" in {
        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              partnerEarnMoreThanNMW = Some(true),
              partnerAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.PARTNER)))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              partnerAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerEarnMoreThanNMW = Some(true)))
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "true")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(maximumEarningsPartnerPath))
      }

      s"only partner not satisfy minimum earnings, redirect to ${selfEmployedOrApprenticePartnerPath}" in {
        when(
          sut.keystore.fetch[PageObjects]()(any(), any())
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.PARTNER),
              parentEarnMoreThanNMW = Some(true),
              partnerEarnMoreThanNMW = Some(true)))
          )
        )

        when(
          sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true,
              parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
              partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
              parentEarnMoreThanNMW = Some(true),
              whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.PARTNER),
              partnerEarnMoreThanNMW = Some(true)))
          )
        )

        val result = await(
          sut.onSubmit(true)(
            request
              .withFormUrlEncodedBody(minimumEarningsKey -> "false")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) should be(Some(selfEmployedOrApprenticePartnerPath))
      }

    }
  }

  "saving in keystore is successful as a parent" should {

    "no data in keystore, redirect to techincal difficulties page" in {
      when(
        sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
      ).thenReturn(
        Future.successful(
          None
        )
      )

      val result = await(
        sut.onSubmit(false)(
          request
            .withFormUrlEncodedBody(minimumEarningsKey -> "true")
            .withSession(validSession)
        )
      )
      status(result) shouldBe SEE_OTHER
      redirectLocation(result) should be(Some(technicalDifficultiesPath))
    }

    s"both in paid employment, redirect to ${minimumEarningsPartnerPath}" in {
      val model = buildPageObjects(isPartner = true,
        parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
        partnerAgeRange = Some(AgeRangeEnum.OVERTWENTYFOUR),
        parentEarnMoreThanNMW = Some(false),
        whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH))

      when(
        sut.keystore.fetch[PageObjects]()(any(), any())
      ).thenReturn(
        Future.successful(
          Some(model.copy(household = model.household.copy(parent = model.household.parent.copy(minimumEarnings = None))))
        )
      )

      when(
        sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
      ).thenReturn(
        Future.successful(
          Some(model)
        )
      )

      val result = await(
        sut.onSubmit(false)(
          request
            .withFormUrlEncodedBody(minimumEarningsKey -> "false")
            .withSession(validSession)
        )
      )
      status(result) shouldBe SEE_OTHER
      redirectLocation(result) should be(Some(minimumEarningsPartnerPath))
    }

    s"only parent not satisfy minimum earnings, redirect to ${selfEmployedOrApprenticeParentPath}" in {
      val po = buildPageObjects(isPartner = false,
        parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
        parentEarnMoreThanNMW = Some(true))
      when(
        sut.keystore.fetch[PageObjects]()(any(), any())
      ).thenReturn(
        Future.successful(
          Some(po)
        )
      )

      when(
        sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
      ).thenReturn(
        Future.successful(
          Some(buildPageObjects(isPartner = false,
            parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
            parentEarnMoreThanNMW = Some(true)))
        )
      )

      val result = await(
        sut.onSubmit(false)(
          request
            .withFormUrlEncodedBody(minimumEarningsKey -> "false")
            .withSession(validSession)
        )
      )
      status(result) shouldBe SEE_OTHER
      redirectLocation(result) should be(Some(selfEmployedOrApprenticeParentPath))
    }

    s"only parent satisfy minimum earnings, redirect to ${maximumEarningsParentPath}" in {
      val po = buildPageObjects(isPartner = false,
        parentAgeRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR),
        whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.YOU),
        parentEarnMoreThanNMW = Some(true))
      when(
        sut.keystore.fetch[PageObjects]()(any(), any())
      ).thenReturn(
        Future.successful(
          Some(po)
        )
      )

      when(
        sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
      ).thenReturn(
        Future.successful(
          Some(po)
        )
      )

      val result = await(
        sut.onSubmit(false)(
          request
            .withFormUrlEncodedBody(minimumEarningsKey -> "true")
            .withSession(validSession)
        )
      )
      status(result) shouldBe SEE_OTHER
      redirectLocation(result) should be(Some(maximumEarningsParentPath))
    }

  }

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
      PageObjects(whichOfYouInPaidEmployment = whichOfYouInPaidEmployment,
        household = Household(location = LocationEnum.ENGLAND, parent = parent, partner = Some(partner)))
    } else {
      PageObjects(whichOfYouInPaidEmployment = whichOfYouInPaidEmployment,
        household = Household(location = LocationEnum.ENGLAND, parent = parent))
    }
  }

}