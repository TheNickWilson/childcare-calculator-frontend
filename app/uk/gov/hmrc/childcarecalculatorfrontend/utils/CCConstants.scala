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

package uk.gov.hmrc.childcarecalculatorfrontend.utils

trait CCConstants {
  lazy val pageObjectsKey = "pageObjects"
  lazy val householdKey = "household"
  lazy val locationKey = "location"
  lazy val childAgedTwoKey = "childAgedTwo"
  lazy val childAgedThreeOrFourKey = "childAgedThreeOrFour"
  lazy val expectChildcareCostsKey = "expectChildcareCosts"
  lazy val childrenKey = "children"
  lazy val parentKey = "parent"
  lazy val livingWithPartnerKey = "livingWithPartner"
  lazy val paidEmploymentKey = "paidEmployment"
  lazy val whichOfYouInPaidEmploymentKey = "whichOfYouInPaidEmployment"
  lazy val WhichBenefitsDoYouGetKey = "whichBenefitsDoYouGet"
  lazy val hoursKey = "hours"
  lazy val vouchersKey = "vouchers"
  lazy val whoGetsVouchersKey = "whoGetsVouchers"
  lazy val getBenefitsKey = "getBenefits"
  lazy val whoGetsBeneftsKey = "whoGetsBenefits"
  lazy val whatsYourAgeKey = "whatsYourAge"
  lazy val minimumEarningsKey = "minimumEarnings"
  lazy val selfEmployedOrApprenticeKey = "selfEmployedOrApprentice"
  lazy val selfEmployedKey = "selfEmployed"
  lazy val maximumEarningsKey = "maximumEarnings"

  def getFamilyStatus(hasPartner: Boolean): String = {
    if(hasPartner) "couple" else "single"
  }

  def getUserType(isPartner: Boolean): String = {
    if(isPartner) "partner" else "parent"
  }

}

object CCConstants extends CCConstants
