#nullable enable
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
	[Serializable]
	[DataContract]
	[XmlRoot(nameof(DrillingPermitMaster))]
	[Table(nameof(DrillingPermitMaster))]
	public class DrillingPermitMaster	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-PERMIT-SEGMENT.

		//03 DA-PERMIT-KEY.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_PERMIT_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_SEQUENCE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PERMIT_SEQUENCE_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_COUNTY_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? DA_PERMIT_COUNTY_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_LEASE_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_PERMIT_LEASE_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PERMIT_DISTRICT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_PERMIT_WELL_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_TOTAL_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_PERMIT_TOTAL_DEPTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_OPERATOR_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_PERMIT_OPERATOR_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_TYPE_APPLICATION), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_TYPE_APPLICATION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_OTHER_EXPLANATION), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_OTHER_EXPLANATION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ADDRESS_UNIQUE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_ADDRESS_UNIQUE_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-ZIP-CODE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ZIP_CODE_PREFIX), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_ZIP_CODE_PREFIX { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ZIP_CODE_SUFFIX), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? DA_ZIP_CODE_SUFFIX { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FICHE_SET_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_FICHE_SET_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ONSHORE_COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? DA_ONSHORE_COUNTY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-RECEIVED-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RECEIVED_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_RECEIVED_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RECEIVED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_RECEIVED_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RECEIVED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_RECEIVED_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RECEIVED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_RECEIVED_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PERMIT-ISSUED-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_ISSUED_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_ISSUED_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_ISSUED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_ISSUED_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_ISSUED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_ISSUED_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_ISSUED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_ISSUED_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PERMIT-AMENDED-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_AMENDED_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_AMENDED_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_AMENDED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_AMENDED_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_AMENDED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_AMENDED_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_AMENDED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_AMENDED_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PERMIT-EXTENDED-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXTENDED_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXTENDED_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXTENDED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXTENDED_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXTENDED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXTENDED_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXTENDED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXTENDED_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PERMIT-SPUD-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SPUD_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SPUD_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SPUD_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SPUD_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SPUD_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SPUD_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SPUD_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SPUD_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PERMIT-SURFACE-CASING-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SURFACE_CASING_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SURFACE_CASING_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SURFACE_CASING_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SURFACE_CASING_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SURFACE_CASING_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SURFACE_CASING_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_SURFACE_CASING_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_SURFACE_CASING_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_WELL_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_WELL_STATUS { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-1995-WELL-STATUS REDEFINES DA-WELL-STATUS PIC X(1).

		//03 DA-PERMIT-WELL-STATUS-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_WELL_STATUS_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_WELL_STATUS_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_WELL_STATUS_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_WELL_STATUS_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_WELL_STATUS_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_WELL_STATUS_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_WELL_STATUS_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_WELL_STATUS_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PERMIT-EXPIRED-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXPIRED_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXPIRED_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXPIRED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXPIRED_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXPIRED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXPIRED_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_EXPIRED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_EXPIRED_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PERMIT-CANCELLED-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_CANCELLED_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_CANCELLED_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_CANCELLED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_CANCELLED_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_CANCELLED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_CANCELLED_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PMT_CANCELLED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_PMT_CANCELLED_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CANCELLATION_REASON), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_CANCELLATION_REASON { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_P12_FILED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_P12_FILED_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SUBSTANDARD_ACREAGE_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_SUBSTANDARD_ACREAGE_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RULE_36_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_RULE_36_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_H9_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_H9_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RULE_37_CASE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_RULE_37_CASE_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RULE_38_DOCKET_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_RULE_38_DOCKET_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_LOCATION_FORMATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_LOCATION_FORMATION_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-OLD-SURFACE-LOCATION.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_OLD_LOCATION), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_OLD_LOCATION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-NEW-SURFACE-LOCATION REDEFINES

		//05 DA-SURFACE-SECTION PIC X(08).

		//05 DA-SURFACE-BLOCK PIC X(10).

		//05 DA-SURFACE-SURVEY PIC X(55).

		//05 DA-SURFACE-ABSTRACT PIC X(06).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_ACRES), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_SURFACE_ACRES { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_MILES_FROM_CITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public float? DA_SURFACE_MILES_FROM_CITY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_DIRECTION_FROM_CITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_SURFACE_DIRECTION_FROM_CITY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_NEAREST_CITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_SURFACE_NEAREST_CITY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-SURFACE-LEASE-DISTANCE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_LEASE_FEET_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_SURFACE_LEASE_FEET_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_LEASE_DIRECTION_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_SURFACE_LEASE_DIRECTION_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_LEASE_FEET_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_SURFACE_LEASE_FEET_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_LEASE_DIRECTION_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_SURFACE_LEASE_DIRECTION_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-OLD-LEASE-DISTANCE-FORMAT REDEFINES

		//05 DA-OLD-SURFACE-LEASE-DISTANCE PIC X(28).

		//03 DA-SURFACE-SURVEY-DISTANCE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_SURVEY_FEET_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_SURFACE_SURVEY_FEET_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_SURVEY_DIRECTION_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_SURFACE_SURVEY_DIRECTION_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_SURVEY_FEET_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_SURFACE_SURVEY_FEET_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SURFACE_SURVEY_DIRECTION_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_SURFACE_SURVEY_DIRECTION_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-OLD-SURVEY-DISTANCE-FORMAT REDEFINES

		//05 DA-OLD-SURFACE-SURVEY-DISTANCE PIC X(28).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_NEAREST_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_NEAREST_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-NEAREST-WELL-NEW-FORMAT REDEFINES DA-NEAREST-WELL.

		//05 DA-NEAREST-WELL-FEET PIC 9(06)V9(2).

		//05 DA-NEAREST-WELL-DIRECTION PIC X(13).

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_NEAREST_WELL_FORMAT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_NEAREST_WELL_FORMAT_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-FINAL-UPDATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FINAL_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FINAL_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FINAL_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FINAL_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FINAL_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FINAL_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FINAL_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FINAL_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CANCELLED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_CANCELLED_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SPUD_IN_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_SPUD_IN_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_DIRECTIONAL_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_DIRECTIONAL_WELL_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_SIDETRACK_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_SIDETRACK_WELL_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_MOVED_INDICATOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_MOVED_INDICATOR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT_CONV_ISSUED_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_PERMIT_CONV_ISSUED_DATE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RULE_37_GRANTED_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_RULE_37_GRANTED_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_HORIZONTAL_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_HORIZONTAL_WELL_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_DUPLICATE_PERMIT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_DUPLICATE_PERMIT_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_NEAREST_LEASE_LINE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_NEAREST_LEASE_LINE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(API_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? API_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitMaster()
		{
		}

		public DrillingPermitMaster(ReadOnlySpan<byte> source)
		{
			//02 DA-PERMIT-SEGMENT.
			//03 DA-PERMIT-KEY.
			DA_PERMIT_NUMBER              = StringParser.ReadAsInt64(source, 3 - 1, 7);
			DA_PERMIT_SEQUENCE_NUMBER              = StringParser.ReadAsInt16(source, 10 - 1, 2);
			DA_PERMIT_COUNTY_CODE              = StringParser.ReadAsInt32(source, 12 - 1, 3);
			DA_PERMIT_LEASE_NAME              = StringParser.ReadAsString(source, 15 - 1, 32);
			DA_PERMIT_DISTRICT              = StringParser.ReadAsInt16(source, 47 - 1, 2);
			DA_PERMIT_WELL_NUMBER              = StringParser.ReadAsString(source, 49 - 1, 6);
			DA_PERMIT_TOTAL_DEPTH              = StringParser.ReadAsInt64(source, 55 - 1, 5);
			DA_PERMIT_OPERATOR_NUMBER              = StringParser.ReadAsInt64(source, 60 - 1, 6);
			DA_TYPE_APPLICATION              = StringParser.ReadAsString(source, 66 - 1, 2);
			DA_OTHER_EXPLANATION              = StringParser.ReadAsString(source, 68 - 1, 30);
			DA_ADDRESS_UNIQUE_NUMBER              = StringParser.ReadAsInt64(source, 98 - 1, 6);
			//03 DA-ZIP-CODE.
			DA_ZIP_CODE_PREFIX              = StringParser.ReadAsInt64(source, 104 - 1, 5);
			DA_ZIP_CODE_SUFFIX              = StringParser.ReadAsInt32(source, 109 - 1, 4);
			DA_FICHE_SET_NUMBER              = StringParser.ReadAsInt64(source, 113 - 1, 6);
			DA_ONSHORE_COUNTY              = StringParser.ReadAsInt32(source, 119 - 1, 3);
			//03 DA-RECEIVED-DATE.
			DA_RECEIVED_CENTURY              = StringParser.ReadAsInt16(source, 122 - 1, 2);
			DA_RECEIVED_YEAR              = StringParser.ReadAsInt16(source, 124 - 1, 2);
			DA_RECEIVED_MONTH              = StringParser.ReadAsInt16(source, 126 - 1, 2);
			DA_RECEIVED_DAY              = StringParser.ReadAsInt16(source, 128 - 1, 2);
			//03 DA-PERMIT-ISSUED-DATE.
			DA_PMT_ISSUED_CENTURY              = StringParser.ReadAsInt16(source, 130 - 1, 2);
			DA_PMT_ISSUED_YEAR              = StringParser.ReadAsInt16(source, 132 - 1, 2);
			DA_PMT_ISSUED_MONTH              = StringParser.ReadAsInt16(source, 134 - 1, 2);
			DA_PMT_ISSUED_DAY              = StringParser.ReadAsInt16(source, 136 - 1, 2);
			//03 DA-PERMIT-AMENDED-DATE.
			DA_PMT_AMENDED_CENTURY              = StringParser.ReadAsInt16(source, 138 - 1, 2);
			DA_PMT_AMENDED_YEAR              = StringParser.ReadAsInt16(source, 140 - 1, 2);
			DA_PMT_AMENDED_MONTH              = StringParser.ReadAsInt16(source, 142 - 1, 2);
			DA_PMT_AMENDED_DAY              = StringParser.ReadAsInt16(source, 144 - 1, 2);
			//03 DA-PERMIT-EXTENDED-DATE.
			DA_PMT_EXTENDED_CENTURY              = StringParser.ReadAsInt16(source, 146 - 1, 2);
			DA_PMT_EXTENDED_YEAR              = StringParser.ReadAsInt16(source, 148 - 1, 2);
			DA_PMT_EXTENDED_MONTH              = StringParser.ReadAsInt16(source, 150 - 1, 2);
			DA_PMT_EXTENDED_DAY              = StringParser.ReadAsInt16(source, 152 - 1, 2);
			//03 DA-PERMIT-SPUD-DATE.
			DA_PMT_SPUD_CENTURY              = StringParser.ReadAsInt16(source, 154 - 1, 2);
			DA_PMT_SPUD_YEAR              = StringParser.ReadAsInt16(source, 156 - 1, 2);
			DA_PMT_SPUD_MONTH              = StringParser.ReadAsInt16(source, 158 - 1, 2);
			DA_PMT_SPUD_DAY              = StringParser.ReadAsInt16(source, 160 - 1, 2);
			//03 DA-PERMIT-SURFACE-CASING-DATE.
			DA_PMT_SURFACE_CASING_CENTURY              = StringParser.ReadAsInt16(source, 162 - 1, 2);
			DA_PMT_SURFACE_CASING_YEAR              = StringParser.ReadAsInt16(source, 164 - 1, 2);
			DA_PMT_SURFACE_CASING_MONTH              = StringParser.ReadAsInt16(source, 166 - 1, 2);
			DA_PMT_SURFACE_CASING_DAY              = StringParser.ReadAsInt16(source, 168 - 1, 2);
			DA_WELL_STATUS              = StringParser.ReadAsChar(source, 170 - 1, 1);
			//03 DA-1995-WELL-STATUS REDEFINES DA-WELL-STATUS PIC X(1).
			//03 DA-PERMIT-WELL-STATUS-DATE.
			DA_PMT_WELL_STATUS_CENTURY              = StringParser.ReadAsInt16(source, 171 - 1, 2);
			DA_PMT_WELL_STATUS_YEAR              = StringParser.ReadAsInt16(source, 173 - 1, 2);
			DA_PMT_WELL_STATUS_MONTH              = StringParser.ReadAsInt16(source, 175 - 1, 2);
			DA_PMT_WELL_STATUS_DAY              = StringParser.ReadAsInt16(source, 177 - 1, 2);
			//03 DA-PERMIT-EXPIRED-DATE.
			DA_PMT_EXPIRED_CENTURY              = StringParser.ReadAsInt16(source, 179 - 1, 2);
			DA_PMT_EXPIRED_YEAR              = StringParser.ReadAsInt16(source, 181 - 1, 2);
			DA_PMT_EXPIRED_MONTH              = StringParser.ReadAsInt16(source, 183 - 1, 2);
			DA_PMT_EXPIRED_DAY              = StringParser.ReadAsInt16(source, 185 - 1, 2);
			//03 DA-PERMIT-CANCELLED-DATE.
			DA_PMT_CANCELLED_CENTURY              = StringParser.ReadAsInt16(source, 187 - 1, 2);
			DA_PMT_CANCELLED_YEAR              = StringParser.ReadAsInt16(source, 189 - 1, 2);
			DA_PMT_CANCELLED_MONTH              = StringParser.ReadAsInt16(source, 191 - 1, 2);
			DA_PMT_CANCELLED_DAY              = StringParser.ReadAsInt16(source, 193 - 1, 2);
			DA_CANCELLATION_REASON              = StringParser.ReadAsString(source, 195 - 1, 30);
			DA_P12_FILED_FLAG              = StringParser.ReadAsChar(source, 225 - 1, 1);
			DA_SUBSTANDARD_ACREAGE_FLAG              = StringParser.ReadAsChar(source, 226 - 1, 1);
			DA_RULE_36_FLAG              = StringParser.ReadAsChar(source, 227 - 1, 1);
			DA_H9_FLAG              = StringParser.ReadAsChar(source, 228 - 1, 1);
			DA_RULE_37_CASE_NUMBER              = StringParser.ReadAsInt64(source, 229 - 1, 7);
			DA_RULE_38_DOCKET_NUMBER              = StringParser.ReadAsInt64(source, 236 - 1, 7);
			DA_LOCATION_FORMATION_FLAG              = StringParser.ReadAsChar(source, 243 - 1, 1);
			//03 DA-OLD-SURFACE-LOCATION.
			DA_OLD_LOCATION              = StringParser.ReadAsString(source, 244 - 1, 52);
			//03 DA-NEW-SURFACE-LOCATION REDEFINES
			//05 DA-SURFACE-SECTION PIC X(08).
			//05 DA-SURFACE-BLOCK PIC X(10).
			//05 DA-SURFACE-SURVEY PIC X(55).
			//05 DA-SURFACE-ABSTRACT PIC X(06).
			DA_SURFACE_ACRES              = StringParser.ReadAsDouble(source, 2, 326 - 1, 8);
			DA_SURFACE_MILES_FROM_CITY              = StringParser.ReadAsSingle(source, 2, 334 - 1, 6);
			DA_SURFACE_DIRECTION_FROM_CITY              = StringParser.ReadAsString(source, 340 - 1, 6);
			DA_SURFACE_NEAREST_CITY              = StringParser.ReadAsString(source, 346 - 1, 13);
			//03 DA-SURFACE-LEASE-DISTANCE.
			DA_SURFACE_LEASE_FEET_1              = StringParser.ReadAsDouble(source, 2, 359 - 1, 8);
			DA_SURFACE_LEASE_DIRECTION_1              = StringParser.ReadAsString(source, 367 - 1, 13);
			DA_SURFACE_LEASE_FEET_2              = StringParser.ReadAsDouble(source, 2, 380 - 1, 8);
			DA_SURFACE_LEASE_DIRECTION_2              = StringParser.ReadAsString(source, 388 - 1, 13);
			//03 DA-OLD-LEASE-DISTANCE-FORMAT REDEFINES
			//05 DA-OLD-SURFACE-LEASE-DISTANCE PIC X(28).
			//03 DA-SURFACE-SURVEY-DISTANCE.
			DA_SURFACE_SURVEY_FEET_1              = StringParser.ReadAsDouble(source, 2, 401 - 1, 8);
			DA_SURFACE_SURVEY_DIRECTION_1              = StringParser.ReadAsString(source, 409 - 1, 13);
			DA_SURFACE_SURVEY_FEET_2              = StringParser.ReadAsDouble(source, 2, 422 - 1, 8);
			DA_SURFACE_SURVEY_DIRECTION_2              = StringParser.ReadAsString(source, 430 - 1, 13);
			//03 DA-OLD-SURVEY-DISTANCE-FORMAT REDEFINES
			//05 DA-OLD-SURFACE-SURVEY-DISTANCE PIC X(28).
			DA_NEAREST_WELL              = StringParser.ReadAsString(source, 443 - 1, 28);
			//03 DA-NEAREST-WELL-NEW-FORMAT REDEFINES DA-NEAREST-WELL.
			//05 DA-NEAREST-WELL-FEET PIC 9(06)V9(2).
			//05 DA-NEAREST-WELL-DIRECTION PIC X(13).
			DA_NEAREST_WELL_FORMAT_FLAG              = StringParser.ReadAsChar(source, 471 - 1, 1);
			//03 DA-FINAL-UPDATE.
			DA_FINAL_CENTURY              = StringParser.ReadAsInt16(source, 472 - 1, 2);
			DA_FINAL_YEAR              = StringParser.ReadAsInt16(source, 474 - 1, 2);
			DA_FINAL_MONTH              = StringParser.ReadAsInt16(source, 476 - 1, 2);
			DA_FINAL_DAY              = StringParser.ReadAsInt16(source, 478 - 1, 2);
			DA_CANCELLED_FLAG              = StringParser.ReadAsChar(source, 480 - 1, 1);
			DA_SPUD_IN_FLAG              = StringParser.ReadAsChar(source, 481 - 1, 1);
			DA_DIRECTIONAL_WELL_FLAG              = StringParser.ReadAsChar(source, 482 - 1, 1);
			DA_SIDETRACK_WELL_FLAG              = StringParser.ReadAsChar(source, 483 - 1, 1);
			DA_MOVED_INDICATOR              = StringParser.ReadAsChar(source, 484 - 1, 1);
			DA_PERMIT_CONV_ISSUED_DATE              = StringParser.ReadAsInt64(source, 485 - 1, 8);
			DA_RULE_37_GRANTED_CODE              = StringParser.ReadAsChar(source, 493 - 1, 1);
			DA_HORIZONTAL_WELL_FLAG              = StringParser.ReadAsChar(source, 494 - 1, 1);
			DA_DUPLICATE_PERMIT_FLAG              = StringParser.ReadAsChar(source, 495 - 1, 1);
			DA_NEAREST_LEASE_LINE              = StringParser.ReadAsString(source, 496 - 1, 7);
			API_NUMBER              = StringParser.ReadAsInt64(source, 503 - 1, -265);
		}
	}
}
