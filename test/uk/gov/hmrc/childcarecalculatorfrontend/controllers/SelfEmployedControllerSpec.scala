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
import uk.gov.hmrc.childcarecalculatorfrontend.models.YouPartnerBothEnum.YouPartnerBothEnum
import uk.gov.hmrc.childcarecalculatorfrontend.models._
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class SelfEmployedControllerSpec extends ControllersValidator with BeforeAndAfterEach {

  val selfEmployedController = new SelfEmployedController(applicationMessagesApi) {
    override val keystore: KeystoreService = mock[KeystoreService]
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(selfEmployedController.keystore)
  }

  validateUrl(selfEmployedParentPath)
  validateUrl(selfEmployedPartnerPath)

  def buildPageObjects(isPartner: Boolean,
                       parentEarnMoreThanNMW: Option[Boolean] = None,
                       partnerEarnMoreThanNMW: Option[Boolean] = None,
                       parentSelfEmployedIn12Months: Option[Boolean] = None,
                       partnerSelfEmployedIn12Months: Option[Boolean] = None,
                       whichOfYouInPaidEmployment: Option[YouPartnerBothEnum] = None
                      ): PageObjects = {
    val parent = Claimant(minimumEarnings = Some(MinimumEarnings(earnMoreThanNMW = parentEarnMoreThanNMW,
      selfEmployedIn12Months = parentSelfEmployedIn12Months)))
    val partner = Claimant(minimumEarnings = Some(MinimumEarnings(earnMoreThanNMW = partnerEarnMoreThanNMW,
      selfEmployedIn12Months = partnerSelfEmployedIn12Months)))
    if (isPartner) {
      PageObjects(whichOfYouInPaidEmployment = whichOfYouInPaidEmployment, household = Household(location = LocationEnum.ENGLAND, parent = parent,
        partner = Some(partner)))
    } else {
      PageObjects(whichOfYouInPaidEmployment = whichOfYouInPaidEmployment, household = Household(location = LocationEnum.ENGLAND, parent = parent))
    }
  }

  private def setupMocks(modelToFetch: Option[PageObjects] = None,
                         modelToStore: Option[PageObjects] = None,
                         fetchPageObjects: Boolean = true,
                         storePageObjects: Boolean = false) = {
    if (fetchPageObjects) {
      when(
        selfEmployedController.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
      ).thenReturn(
          Future.successful(modelToFetch)
        )
    }

    if (storePageObjects) {
      when(
        selfEmployedController.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
      ).thenReturn(
          Future.successful(modelToStore)
        )

    }
  }

  private def setupMocksForException() = {
    when(
      selfEmployedController.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
    ).thenReturn(
        Future.failed(new RuntimeException)
      )
  }

  "SelfEmployedController" when {

    "onPageLoad is called" should {

      "redirect successfully if there is no data in keystore if parent" in {
        setupMocks()
        val result = await(selfEmployedController.onPageLoad(false)(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "redirect successfully if there is no data in keystore if partner" in {
        setupMocks()
        val result = await(selfEmployedController.onPageLoad(true)(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "load successfully parent template" in {
        setupMocks(modelToFetch = Some(buildPageObjects(isPartner = false, parentSelfEmployedIn12Months = Some(true))))
        val result = await(selfEmployedController.onPageLoad(false)(request.withSession(validSession)))
        status(result) shouldBe OK
      }

      "load successfully partner template" in {
        setupMocks(Some(buildPageObjects(isPartner = true, partnerSelfEmployedIn12Months = Some(true))))

        when(
          selfEmployedController.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = true, partnerSelfEmployedIn12Months = Some(true)))
          )
        )
        val result = await(selfEmployedController.onPageLoad(true)(request.withSession(validSession)))
        status(result) shouldBe OK
      }

      "load template successfully if there is data in keystore for parent and define correctly backUrl" when {
        "redirect to parent's self or apprentice page" in {
          setupMocks(Some(buildPageObjects(isPartner = false, parentEarnMoreThanNMW = Some(false))))
          val result = await(selfEmployedController.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe selfEmployedOrApprenticeParentPath
        }

        "redirect to error page if can't connect with keystore" in {
          setupMocksForException()
          val result = await(selfEmployedController.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe technicalDifficultiesPath
        }
      }

      "load template successfully if there is data in keystore for partner and display correct backUrl" when {
        "redirect to partner's self or apprentice page" in {
          setupMocks(Some(buildPageObjects(isPartner = true,
                partnerEarnMoreThanNMW = Some(false)))
            )
          val result = await(selfEmployedController.onPageLoad(true)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe
                        routes.SelfEmployedOrApprenticeController.onPageLoad(true).url
        }

        "redirect to error page if can't connect with keystore if partner" in {
          setupMocksForException()
          val result = await(selfEmployedController.onPageLoad(true)(request.withSession(validSession)))
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe technicalDifficultiesPath
        }
      }

    }

    "onSubmit is called" when {

      "connecting with keystore fails" in {
        setupMocks(modelToFetch = Some(buildPageObjects(true, None)))
        setupMocksForException()
        val result = await(
          selfEmployedController.onSubmit(true)(
            request
              .withFormUrlEncodedBody(selfEmployedKey -> "true")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "there are errors" should {
        "load same template and return BAD_REQUEST as a partner" in {
          setupMocks(modelToFetch = Some(buildPageObjects(true,
            partnerEarnMoreThanNMW = Some(false)
          )))
          val result = await(
            selfEmployedController.onSubmit(true)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "")
                .withSession(validSession)
            )
          )
          status(result) shouldBe BAD_REQUEST
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
        }

        "load same template and return BAD_REQUEST as a parent" in {
          setupMocks(modelToFetch = Some(buildPageObjects(false)))

          val result = await(
            selfEmployedController.onSubmit(false)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "")
                .withSession(validSession)
            )
          )
          status(result) shouldBe BAD_REQUEST
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
        }
      }

      "saving in keystore is successful as a partner with SelfEmployed = true" should {

        s"there is no data in keystore for PageObjects object for partner" in {
          setupMocks()
          val result = await(
            selfEmployedController.onSubmit(true)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "123")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe technicalDifficultiesPath
        }

        s"there is data in keystore and is self employed for less than 12 months, redirect to parent's max earnings page" in {
          val model = buildPageObjects(isPartner = true,
            whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
            parentEarnMoreThanNMW = Some(true),
            partnerSelfEmployedIn12Months = Some(false))

          val modelToStore = buildPageObjects(isPartner = true,
                parentEarnMoreThanNMW = Some(true),
                whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
                partnerSelfEmployedIn12Months = Some(true)
              )

          setupMocks(modelToFetch = Some(model), modelToStore = Some(modelToStore), storePageObjects = true)

          val result = await(
            selfEmployedController.onSubmit(true)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "true")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe maximumEarningsParentPath
        }

      }

      "saving in keystore is successful as a partner with SelfEmployed = false" should {

        s"redirect to ${creditsPath} page when both are in paid employment" in {
          val model = buildPageObjects(isPartner = true,
            partnerSelfEmployedIn12Months = None,
            whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH))

          val modelToStore = buildPageObjects(isPartner = true,
                partnerSelfEmployedIn12Months = Some(false),
                whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH))

          setupMocks(modelToFetch = Some(model), modelToStore = Some(modelToStore), storePageObjects = true)

          val result = await(
            selfEmployedController.onSubmit(true)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "false")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe creditsPath
        }

        s"redirect to ${creditsPath} page when only partner is in paid employment" in {
          val model = buildPageObjects(isPartner = true,
            partnerSelfEmployedIn12Months = None,
            whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.PARTNER))

          val modelToStore = buildPageObjects(isPartner = true,
                partnerSelfEmployedIn12Months = Some(false),
                whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.PARTNER))

          setupMocks(modelToFetch = Some(model), modelToStore = Some(modelToStore), storePageObjects = true)


          val result = await(
            selfEmployedController.onSubmit(true)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "false")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe creditsPath
        }
      }

      "saving in keystore is successful as a parent with SelfEmployed = true" should {

        s"there is data in keystore, redirect to partner's self or apprentice page" in {
          val model = buildPageObjects(isPartner = true,
            whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
            partnerEarnMoreThanNMW = Some(false),
            parentSelfEmployedIn12Months = None)

          val modelToStore = buildPageObjects(isPartner = true,
                partnerEarnMoreThanNMW = Some(false),
                whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
                parentSelfEmployedIn12Months = Some(true))

          setupMocks(modelToFetch = Some(model), modelToStore = Some(modelToStore), storePageObjects = true)

          val result = await(
            selfEmployedController.onSubmit(false)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "true")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe routes.SelfEmployedOrApprenticeController.onPageLoad(true).url
        }

        s"there is data in keystore, redirect to partner's max earnings page" in {
          val model = buildPageObjects(isPartner = true,
            whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
            parentEarnMoreThanNMW = Some(false),
            partnerEarnMoreThanNMW = Some(true),
            parentSelfEmployedIn12Months = Some(true))

          val modelToStore = buildPageObjects(isPartner = true,
                whichOfYouInPaidEmployment = Some(YouPartnerBothEnum.BOTH),
                parentEarnMoreThanNMW = Some(false),
                partnerEarnMoreThanNMW = Some(true),
                parentSelfEmployedIn12Months = Some(true))

          setupMocks(modelToFetch = Some(model), modelToStore = Some(modelToStore), storePageObjects = true)

          val result = await(
            selfEmployedController.onSubmit(false)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "true")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          redirectLocation(result) shouldBe Some(maximumEarningsPartnerPath)
        }

      }

     "saving in keystore is successful as a parent and SelfEmployed = false" should {

        s"there is no data in keystore for PageObjects object for parent" in {
          setupMocks()

          val result = await(
            selfEmployedController.onSubmit(false)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "false")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe technicalDifficultiesPath
        }

        s"redirect to ${creditsPath} page" in {
          val model = buildPageObjects(isPartner = false, parentSelfEmployedIn12Months = Some(false))
          
          setupMocks(modelToFetch = Some(model), modelToStore = Some(model), storePageObjects = true)

          val result = await(
            selfEmployedController.onSubmit(false)(
              request
                .withFormUrlEncodedBody(selfEmployedKey -> "false")
                .withSession(validSession)
            )
          )
          status(result) shouldBe SEE_OTHER
          result.header.headers("Location") shouldBe creditsPath
        }

      }
    }

  }
}
