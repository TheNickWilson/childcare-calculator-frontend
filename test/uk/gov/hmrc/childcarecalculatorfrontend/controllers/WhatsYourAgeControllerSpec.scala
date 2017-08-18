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

import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Messages.Implicits.applicationMessagesApi
import play.api.libs.json.{Reads}
import play.api.test.Helpers._
import uk.gov.hmrc.childcarecalculatorfrontend.ControllersValidator
import uk.gov.hmrc.childcarecalculatorfrontend.models.AgeRangeEnum.AgeRangeEnum
import uk.gov.hmrc.childcarecalculatorfrontend.models._
import uk.gov.hmrc.childcarecalculatorfrontend.services.KeystoreService
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class WhatsYourAgeControllerSpec extends ControllersValidator with BeforeAndAfterEach {

  val sut = new WhatsYourAgeController(applicationMessagesApi) {
    override val keystore: KeystoreService = mock[KeystoreService]
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(sut.keystore)
  }

  validateUrl(whatsYourAgePath)

  def buildPageObjects(isPartner: Boolean, ageRange: Option[AgeRangeEnum]): PageObjects = {
    val claimant = Claimant(ageRange = ageRange)
    if(isPartner) {
      PageObjects(household = Household(location = LocationEnum.ENGLAND, parent = claimant))
    } else {
      PageObjects(household = Household(location = LocationEnum.ENGLAND, parent = claimant, partner = Some(claimant)))
    }
  }

  "WhatsYourAgeController" when {

    "onPageLoad is called" should {

      "load template successfully if there is no data in keystore" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            None
          )
        )
        val result = await(sut.onPageLoad(any())(request.withSession(validSession)))
        status(result) shouldBe OK
        result.body.contentType.get shouldBe "text/html; charset=utf-8"
      }

      "load template successfully if there is data in keystore" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.successful(
            Some(buildPageObjects(isPartner = false, ageRange = Some(AgeRangeEnum.TWENTYONETOTWENTYFOUR)))
          )
        )
        val result = await(sut.onPageLoad(any())(request.withSession(validSession)))
        status(result) shouldBe OK
        result.body.contentType.get shouldBe "text/html; charset=utf-8"
      }

      "redirect to error page if can't connect with keystore" in {
        when(
          sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
        ).thenReturn(
          Future.failed(new RuntimeException)
        )
        val result = await(sut.onPageLoad(any())(request.withSession(validSession)))
        status(result) shouldBe SEE_OTHER
        result.header.headers("Location") shouldBe technicalDifficultiesPath
      }
    }
  }
}

//    "onSubmit is called" when {
//
//      "there are errors" should {
//        "load same template and return BAD_REQUEST" in {
//          val result = await(
//            sut.onSubmit(
//              request
//                .withFormUrlEncodedBody(locationKey -> "")
//                .withSession(validSession)
//            )
//          )
//          status(result) shouldBe BAD_REQUEST
//          result.body.contentType.get shouldBe "text/html; charset=utf-8"
//        }
//      }
//
//      "saving in keystore is successful" should {
//        s"go to ${childAgedTwoPath}" when {
//          val childAgeTwoLocations = List(
//            LocationEnum.ENGLAND,
//            LocationEnum.SCOTLAND,
//            LocationEnum.WALES
//          )
//          childAgeTwoLocations.foreach { loc =>
//            s"${loc.toString} is selected if there is no data in keystore for PageObjects object" in {
//              when(
//                sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
//              ).thenReturn(
//                Future.successful(
//                  None
//                )
//              )
//
//              when(
//                sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
//              ).thenReturn(
//                Future.successful(
//                  Some(buildPageObjects(location = loc))
//                )
//              )
//
//              val result = await(
//                sut.onSubmit(
//                  request
//                    .withFormUrlEncodedBody(locationKey -> loc.toString)
//                    .withSession(validSession)
//                )
//              )
//              status(result) shouldBe SEE_OTHER
//              result.header.headers("Location") shouldBe childAgedTwoPath
//            }
//
//            s"${loc.toString} is selected if there is data in keystore for PageObjects object" in {
//              when(
//                sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
//              ).thenReturn(
//                Future.successful(
//                  Some(buildPageObjects(location = LocationEnum.ENGLAND))
//                )
//              )
//
//              when(
//                sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
//              ).thenReturn(
//                Future.successful(
//                  Some(buildPageObjects(location = loc))
//                )
//              )
//
//              val result = await(
//                sut.onSubmit(
//                  request
//                    .withFormUrlEncodedBody(locationKey -> loc.toString)
//                    .withSession(validSession)
//                )
//              )
//              status(result) shouldBe SEE_OTHER
//              result.header.headers("Location") shouldBe childAgedTwoPath
//            }
//          }
//        }
//
//        s"go to '3 or 4 years old page' ${childAgedThreeOrFourPath}" when {
//          val childAgeTwoLocations = List(
//            LocationEnum.NORTHERNIRELAND
//          )
//          childAgeTwoLocations.foreach { loc =>
//            s"${loc.toString} is selected if there is no data in keystore for Househild object" in {
//              when(
//                sut.keystore.fetch[PageObjects]()(any(), any())
//              ).thenReturn(
//                Future.successful(
//                  None
//                )
//              )
//
//              when(
//                sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
//              ).thenReturn(
//                Future.successful(
//                  Some(buildPageObjects(location = loc))
//                )
//              )
//
//              val result = await(
//                sut.onSubmit(
//                  request
//                    .withFormUrlEncodedBody(locationKey -> loc.toString)
//                    .withSession(validSession)
//                )
//              )
//              status(result) shouldBe SEE_OTHER
//              result.header.headers("Location") shouldBe childAgedThreeOrFourPath
//            }
//
//            s"${loc.toString} is selected if there is data in keystore for PageObjects object" in {
//              when(
//                sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
//              ).thenReturn(
//                Future.successful(
//                  Some(buildPageObjects(location = LocationEnum.ENGLAND))
//                )
//              )
//
//              when(
//                sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
//              ).thenReturn(
//                Future.successful(
//                  Some(buildPageObjects(location = loc))
//                )
//              )
//
//              val result = await(
//                sut.onSubmit(
//                  request
//                    .withFormUrlEncodedBody(locationKey -> loc.toString)
//                    .withSession(validSession)
//                )
//              )
//              status(result) shouldBe SEE_OTHER
//              result.header.headers("Location") shouldBe childAgedThreeOrFourPath
//            }
//          }
//        }
//      }
//
//      "connecting with keystore fails" should {
//        s"redirect to ${technicalDifficultiesPath}" in {
//          when(
//            sut.keystore.fetch[PageObjects]()(any[HeaderCarrier], any[Reads[PageObjects]])
//          ).thenReturn(
//            Future.successful(
//              Some(buildPageObjects(location = LocationEnum.ENGLAND))
//            )
//          )
//
//          when(
//            sut.keystore.cache[PageObjects](any[PageObjects])(any[HeaderCarrier], any[Format[PageObjects]])
//          ).thenReturn(
//            Future.failed(new RuntimeException)
//          )
//
//          val result = await(
//            sut.onSubmit(
//              request
//                .withFormUrlEncodedBody(locationKey -> LocationEnum.ENGLAND.toString)
//                .withSession(validSession)
//            )
//          )
//          status(result) shouldBe SEE_OTHER
//          result.header.headers("Location") shouldBe technicalDifficultiesPath
//        }
//      }
//
//    }
//  }
//}
