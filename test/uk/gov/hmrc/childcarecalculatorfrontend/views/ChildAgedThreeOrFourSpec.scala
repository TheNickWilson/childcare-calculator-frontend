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

package uk.gov.hmrc.childcarecalculatorfrontend.views

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import play.api.mvc.Call
import uk.gov.hmrc.childcarecalculatorfrontend.forms.ChildAgedThreeOrFourForm
import uk.gov.hmrc.childcarecalculatorfrontend.views.html.childAgedThreeOrFour
import uk.gov.hmrc.childcarecalculatorfrontend.{FakeCCApplication, TemplatesValidator}
import play.api.test.Helpers._

class ChildAgedThreeOrFourSpec extends TemplatesValidator with FakeCCApplication {

  override val contentData: List[ElementDetails] = List(
    ElementDetails(id = Some("page-title"), value = "Do you have any children aged 3 or 4?"),
    ElementDetails(tagName = Some("p"), tagIndex = Some(0), value = "All 3 and 4 year olds in England are entitled to 15 hours of free early education and childcare a week in term time (570 hours a year)."),
    ElementDetails(attribute = Some("for"), attributeValue = Some("childAgedThreeOrFour-true"), value = "Yes"),
    ElementDetails(attribute = Some("for"), attributeValue = Some("childAgedThreeOrFour-false"), value = "No"),
    ElementDetails(attribute = Some("type"), attributeValue = Some("submit"), checkAttribute = Some("title"), value = ""),
    ElementDetails(id = Some("back-button"), value = "Back")
  )

  override val linksData: List[ElementDetails] = List(
    ElementDetails(elementClass = Some("form"), checkAttribute = Some("action"), value = childAgedThreeOrFourPath),
    ElementDetails(id = Some("back-button"), checkAttribute = Some("href"), value = childAgedTwoPath)
  )

  val backUrl: Call = Call("GET", childAgedTwoPath)

  val location = "england"

  def getTemplate(form: Form[Option[Boolean]], location: String = "england"): Document = {
    val template = childAgedThreeOrFour(form, backUrl, location)(request, applicationMessages)
    Jsoup.parse(contentAsString(template))
  }

  "calling ChildAgedThreeOrFour template" should {
    "render template" in {
      val template = childAgedThreeOrFour.render(new ChildAgedThreeOrFourForm(applicationMessagesApi).form, backUrl, location, request, applicationMessages)
      template.contentType shouldBe "text/html"

      val template1 = childAgedThreeOrFour.f(new ChildAgedThreeOrFourForm(applicationMessagesApi).form, backUrl, location)(request, applicationMessages)
      template1.contentType shouldBe "text/html"
    }

    "display correct content" when {

      "nothing is selected" in {
        implicit val doc: Document = getTemplate(new ChildAgedThreeOrFourForm(applicationMessagesApi).form.fill(None))

        verifyPageContent()
        verifyPageLinks()
        verifyChecks()
        verifyErrors()
      }

      "true is selected" in {
        implicit val doc: Document = getTemplate(new ChildAgedThreeOrFourForm(applicationMessagesApi).form.fill(Some(true)))

        verifyPageContent()
        verifyPageLinks()
        verifyChecks(List(s"${childAgedThreeOrFourKey}-true"))
        verifyErrors()
      }

      "false is selected" in {
        implicit val doc: Document = getTemplate(new ChildAgedThreeOrFourForm(applicationMessagesApi).form.fill(Some(false)))

        verifyPageContent()
        verifyPageLinks()
        verifyChecks(List(s"${childAgedThreeOrFourKey}-false"))
        verifyErrors()
      }

      "form is submitted without data" in {
        val form = new ChildAgedThreeOrFourForm(applicationMessagesApi).form.bind(
          Map(
            childAgedThreeOrFourKey -> ""
          )
        )
        implicit val doc: Document = getTemplate(form)

        verifyPageContent()
        verifyPageLinks()
        verifyChecks()
        verifyErrors(
          errors = Map("childAgedThreeOrFour" -> applicationMessages.messages("child.aged.three.or.four.yes.no.not.selected.error"))
        )
      }

      "form is submitted with invalid data" in {
        val form = new ChildAgedThreeOrFourForm(applicationMessagesApi).form.bind(
          Map(
            childAgedThreeOrFourKey -> "abcd"
          )
        )
        implicit val doc: Document = getTemplate(form)

        verifyPageContent()
        verifyPageLinks()
        verifyChecks()
        verifyErrors(
          errors = Map("childAgedThreeOrFour" -> applicationMessages.messages("error.boolean"))
        )
      }

    }

    "display different content based on location" when {
      "location is scotland" in {
        val location = "scotland"
        val form = new ChildAgedThreeOrFourForm(applicationMessagesApi).form.fill(Some(true))
        val template = getTemplate(form, location)

        template.getElementById("aged-three-four-info").text() shouldBe Messages(s"child.aged.three.or.four.info.scotland")
      }

      "location is wales" in {
        val location = "wales"
        val form = new ChildAgedThreeOrFourForm(applicationMessagesApi).form.fill(Some(true))
        val template = getTemplate(form, location)

        template.getElementById("aged-three-four-info").text() shouldBe Messages(s"child.aged.three.or.four.info.wales")
      }

      "location is northen-ireland" in {
        val location = "northern-ireland"
        val form = new ChildAgedThreeOrFourForm(applicationMessagesApi).form.fill(Some(true))
        val template = getTemplate(form, location)

        template.getElementById("aged-three-four-info").text() shouldBe Messages(s"child.aged.three.or.four.info.northern-ireland")
      }
    }
  }
}
