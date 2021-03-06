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

package uk.gov.hmrc.childcarecalculatorfrontend.models

import play.api.libs.json.{Format, Reads, Writes}
import uk.gov.hmrc.childcarecalculatorfrontend.utils.EnumUtils

object TcUcBenefitsEnum extends Enumeration {
  type TcUcBenefitsEnum = Value
  val TAXCREDITS, UNIVERSALCREDITS, NEITHER = Value

  val enumReads: Reads[TcUcBenefitsEnum] = EnumUtils.enumReads(TcUcBenefitsEnum)

  val enumWrites: Writes[TcUcBenefitsEnum] = EnumUtils.enumWrites

  implicit def enumFormats: Format[TcUcBenefitsEnum] = EnumUtils.enumFormat(TcUcBenefitsEnum)
}

object LocationEnum extends Enumeration {
  type LocationEnum = Value
  val ENGLAND = Value("england")
  val SCOTLAND = Value("scotland")
  val WALES = Value("wales")
  val NORTHERNIRELAND = Value("northern-ireland")

  val enumReads: Reads[LocationEnum] = EnumUtils.enumReads(LocationEnum)

  val enumWrites: Writes[LocationEnum] = EnumUtils.enumWrites

  implicit def enumFormats: Format[LocationEnum] = EnumUtils.enumFormat(LocationEnum)
}

object AgeRangeEnum extends Enumeration {
  type AgeRangeEnum = Value
  val UNDER18, EIGHTEENTOTWENTY, TWENTYONETOTWENTYFOUR, OVERTWENTYFOUR = Value
  val enumReads: Reads[AgeRangeEnum] = EnumUtils.enumReads(AgeRangeEnum)

  val enumWrites: Writes[AgeRangeEnum] = EnumUtils.enumWrites

  implicit def enumFormats: Format[AgeRangeEnum] = EnumUtils.enumFormat(AgeRangeEnum)
}

object EmploymentStatusEnum extends Enumeration {
  type EmploymentStatusEnum = Value
  val SELFEMPLOYED, APPRENTICE, NEITHER = Value

  val enumReads: Reads[EmploymentStatusEnum] = EnumUtils.enumReads(EmploymentStatusEnum)

  val enumWrites: Writes[EmploymentStatusEnum] = EnumUtils.enumWrites

  implicit def enumFormats: Format[EmploymentStatusEnum] = EnumUtils.enumFormat(EmploymentStatusEnum)
}

object YesNoUnsureEnum extends Enumeration {
  type YesNoUnsureEnum = Value
  val YES, NO, NOTSURE = Value

  val enumReads: Reads[YesNoUnsureEnum] = EnumUtils.enumReads(YesNoUnsureEnum)

  val enumWrites: Writes[YesNoUnsureEnum] = EnumUtils.enumWrites

  implicit def enumFormats: Format[YesNoUnsureEnum] = EnumUtils.enumFormat(YesNoUnsureEnum)
  }

object PeriodEnum extends Enumeration {
  type PeriodEnum = Value
  val DAILY, WEEKLY, FORTNIGHTLY, MONTHLY, QUARTERLY, YEARLY, INVALID = Value

  val enumReads: Reads[PeriodEnum] = EnumUtils.enumReads(PeriodEnum)

  val enumWrites: Writes[PeriodEnum] = EnumUtils.enumWrites

  implicit def enumFormats: Format[PeriodEnum] = EnumUtils.enumFormat(PeriodEnum)
}

object SchemeEnum extends Enumeration {
  type SchemeEnum = Value
  val TFCELIGIBILITY = Value("tfcEligibility")
  val TCELIGIBILITY = Value("tcEligibility")
  val ESCELIGIBILITY = Value("escEligibility")

  val enumReads: Reads[SchemeEnum] = EnumUtils.enumReads(SchemeEnum)

  val enumWrites: Writes[SchemeEnum] = EnumUtils.enumWrites

  implicit def enumFormats: Format[SchemeEnum] = EnumUtils.enumFormat(SchemeEnum)
}

object YouPartnerBothEnum extends Enumeration {
  type YouPartnerBothEnum = Value
  val YOU, PARTNER, BOTH = Value

  implicit def enumFormats: Format[YouPartnerBothEnum] = EnumUtils.enumFormat(YouPartnerBothEnum)
}

