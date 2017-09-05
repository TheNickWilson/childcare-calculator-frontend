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

/**
 * Created by user on 05/09/17.
 */

import javax.inject.{Inject, Singleton}

import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import uk.gov.hmrc.childcarecalculatorfrontend.models.YouPartnerBothEnum._
import uk.gov.hmrc.childcarecalculatorfrontend.utils.CCConstants

@Singleton
class MaximumEarningForm @Inject()(hasPartner: Boolean = false, isPartner: Boolean, val messagesApi: MessagesApi) extends I18nSupport with CCConstants {

  val familyStatus: String = getFamilyStatus(hasPartner)
  val user = getUserType(isPartner)

  type MaximumEarningFormType = Option[Boolean]

  val form = Form[MaximumEarningFormType](
    single(
      maximumEarningKey -> optional(boolean).verifying(
        if(familyStatus == "couple") {
          Messages(s"maximum.earning.error.both")
        } else {
          Messages(s"maximum.earning.error.${user}")
        },
        _.isDefined
      )
    )
  )
}
