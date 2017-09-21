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
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.{Format, Reads}
import play.api.test.Helpers._
import uk.gov.hmrc.childcarecalculatorfrontend.ControllersValidator
import uk.gov.hmrc.childcarecalculatorfrontend.models.YouPartnerBothEnum._
import uk.gov.hmrc.childcarecalculatorfrontend.models.{Household, LocationEnum, PageObjects}
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class ChildAgedThreeOrFourControllerSpec extends ControllersValidator with BeforeAndAfterEach {

  val childAgedThreeOrFourController = new ChildAgedThreeOrFourController(applicationMessagesApi) {
    override val keystore: KeystoreService = mock[KeystoreService]
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(childAgedThreeOrFourController.keystore)
  }


  "onPageLoad" should {
    "load successfully ChildAgedThreeOrFour template" when {

      "there is no data in keystore about child aged 3 or 4" should {
        s"contain back url to ${locationPath} if there is no data about child aged 2" in {
          setupMocks(
            Some(
              buildPageObjects(
                childAgedTwo = None,
                childAgedThreeOrFour = None
              )
            )
          )

          val result = await(childAgedThreeOrFourController.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe locationPath
        }

        s"contain back url to ${childAgedTwoPath} if there is data about child aged 2" in {
          setupMocks(
            Some(
              buildPageObjects(
                childAgedTwo = Some(true),
                childAgedThreeOrFour = None
              )
            )
          )

          val result = await(childAgedThreeOrFourController.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe childAgedTwoPath
        }
      }

      "there is data in keystore about child aged 3 or 4" should {
        s"contain back url to ${locationPath} if there is no data about child aged 2" in {
          setupMocks(
            Some(
              buildPageObjects(
                childAgedTwo = None,
                childAgedThreeOrFour = Some(true)
              )
            )
          )

          val result = await(childAgedThreeOrFourController.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe locationPath
        }

        s"contain back url to free-hours-results if there is no data about child aged 2 and summary is true" in {
          setupMocks(
            Some(
              buildPageObjects(
                childAgedTwo = None,
                childAgedThreeOrFour = Some(true)
              )
            )
          )

          val result = await(childAgedThreeOrFourController.onPageLoad(true)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe "/childcare-calc/free-hours-results"
        }

        s"contain back url to ${childAgedTwoPath} if there is data about child aged 2" in {
          setupMocks(
            Some(
              buildPageObjects(
                childAgedTwo = Some(true),
                childAgedThreeOrFour = Some(true)
              )
            )
          )

          val result = await(childAgedThreeOrFourController.onPageLoad(false)(request.withSession(validSession)))
          status(result) shouldBe OK
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe childAgedTwoPath
        }
      }
    }

    s"redirect to technical difficulties page (${technicalDifficultiesPath})" when {
      "there is no data for household in keystore" in {
        setupMocks()

        val result = await(childAgedThreeOrFourController.onPageLoad(false)(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "can't connect to keystore" in {
        setupMocksForException()

        val result = await(childAgedThreeOrFourController.onPageLoad(false)(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }
    }
  }

  "onSubmit" should {
    "load template with status BAD_REQUEST" when {
      "invalid data is submitted" should {
        s"cantain back url to ${locationPath} if there is no data in keystore about child aged 2" in {
          setupMocks(
            Some(
              buildPageObjects(
                childAgedTwo = None,
                childAgedThreeOrFour = None
              )
            )
          )

          val result = await(childAgedThreeOrFourController.onSubmit(request.withSession(validSession)))
          status(result) shouldBe BAD_REQUEST
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe locationPath
        }

        s"contain back url to ${childAgedTwoPath} if there is data in keystore about child aged 2" in {
          setupMocks(
            Some(
              buildPageObjects(
                childAgedTwo = Some(true),
                childAgedThreeOrFour = None
              )
            )
          )
          val result = await(childAgedThreeOrFourController.onSubmit(request.withSession(validSession)))
          status(result) shouldBe BAD_REQUEST
          result.body.contentType.get shouldBe "text/html; charset=utf-8"
          val content = Jsoup.parse(bodyOf(result))
          content.getElementById("back-button").attr("href") shouldBe childAgedTwoPath
        }
      }
    }

    s"redirect to error page (${technicalDifficultiesPath})" when {
      "there is no data in keystore for PageObjects object" in {
        setupMocks()

        val result = await(childAgedThreeOrFourController.onSubmit(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "can't connect to keystore loading PageObjects object" in {
        setupMocksForException()

        val result = await(childAgedThreeOrFourController.onSubmit(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }

      "valid data is submitted and saving in keystore fails" in {
        when(
          childAgedThreeOrFourController.keystore.fetch[PageObjects]()(any(),any())
        ).thenReturn(
          Future.successful(
            Some(
              buildPageObjects(
                childAgedTwo = None,
                childAgedThreeOrFour = None
              )
            )
          )
        )

        when(
          childAgedThreeOrFourController.keystore.cache(any[PageObjects]())(any(),any())
        ).thenReturn(
          Future.failed(new RuntimeException)
        )


        val result = await(
          childAgedThreeOrFourController.onSubmit(
            request
              .withFormUrlEncodedBody(childAgedThreeOrFourKey -> "true")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }
    }

    s"redirect to next page (${expectChildcareCostsPath})" when {
      "valid data is submitted and saving in keystore is successful" in {
        val model = buildPageObjects(
                childAgedTwo = None,
                childAgedThreeOrFour = None
              )

        val modelToStore = buildPageObjects(
                childAgedTwo = None,
                childAgedThreeOrFour = Some(true)
              )

        setupMocks(modelToFetch = Some(model), modelToStore = Some(modelToStore), storePageObjects = true)


        val result = await(
          childAgedThreeOrFourController.onSubmit(
            request
              .withFormUrlEncodedBody(childAgedThreeOrFourKey -> "true")
              .withSession(validSession)
          )
        )
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe expectChildcareCostsPath
      }
    }
  }


  validateUrl(childAgedThreeOrFourPath)

  def buildPageObjects(childAgedTwo: Option[Boolean] = None,
                       childAgedThreeOrFour: Option[Boolean] = None): PageObjects = PageObjects(household = Household(
    location = LocationEnum.ENGLAND),
    childAgedTwo = childAgedTwo,
    childAgedThreeOrFour = childAgedThreeOrFour
  )
  
  private def setupMocks(modelToFetch: Option[PageObjects] = None,
                         modelToStore: Option[PageObjects] = None,
                         fetchPageObjects: Boolean = true,
                         storePageObjects: Boolean = false) = {
    if (fetchPageObjects) {
      when(
        childAgedThreeOrFourController.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
      ).thenReturn(
          Future.successful(modelToFetch)
        )
    }

    if (storePageObjects) {
      when(
        childAgedThreeOrFourController.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
      ).thenReturn(
          Future.successful(modelToStore)
        )

    }
  }

  private def setupMocksForException() = {
    when(
      childAgedThreeOrFourController.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
    ).thenReturn(
        Future.failed(new RuntimeException)
      )
  }
}