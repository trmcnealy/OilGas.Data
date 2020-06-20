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
	[XmlRoot(nameof(DrillingPermitRoot))]
	[Table(nameof(DrillingPermitRoot))]
	public class DrillingPermitRoot	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }

        //02 DA-STATUS-ROOT.

		//03 DA-PRIMARY-KEY.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_STATUS_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_STATUS_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_STATUS_SEQUENCE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_STATUS_SEQUENCE_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-SECONDARY-KEYS.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_COUNTY_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? DA_COUNTY_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_LEASE_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_LEASE_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_DISTRICT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_OPERATOR_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_OPERATOR_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 DA-DATE-APP-RECEIVED.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_APP_RCVD_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_APP_RCVD_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_APP_RCVD_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_APP_RCVD_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_APP_RCVD_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_APP_RCVD_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_APP_RCVD_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_APP_RCVD_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//05 DA-DATE-APP-RCVD-RE REDEFINES

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_OPERATOR_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_OPERATOR_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_HB1407_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_HB1407_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_STATUS_OF_APP_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_STATUS_OF_APP_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-PROBLEM-FLAGS.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_NOT_ENOUGH_MONEY_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_NOT_ENOUGH_MONEY_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_TOO_MUCH_MONEY_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_TOO_MUCH_MONEY_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_P5_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_P5_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_P12_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_P12_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PLAT_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_PLAT_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_W1A_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_W1A_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_OTHER_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_OTHER_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RULE37_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_RULE37_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RULE38_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_RULE38_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_RULE39_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_RULE39_PROBLEM_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_NO_MONEY_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_NO_MONEY_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_PERMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_PERMIT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-ISSUE-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ISSUE_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_ISSUE_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ISSUE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_ISSUE_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ISSUE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_ISSUE_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ISSUE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_ISSUE_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-WITHDRAWN-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_WITHDRAWN_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_WITHDRAWN_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_WITHDRAWN_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_WITHDRAWN_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_WITHDRAWN_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_WITHDRAWN_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_WITHDRAWN_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_WITHDRAWN_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_WALKTHROUGH_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_WALKTHROUGH_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_OTHER_PROBLEM_TEXT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_OTHER_PROBLEM_TEXT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_WELL_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_BUILT_FROM_OLD_MASTER_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_BUILT_FROM_OLD_MASTER_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_STATUS_RENUMBERED_TO), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_STATUS_RENUMBERED_TO { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_STATUS_RENUMBERED_FROM), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_STATUS_RENUMBERED_FROM { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_APPLICATION_RETURNED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_APPLICATION_RETURNED_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ECAP_FILING_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_ECAP_FILING_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitRoot()
		{
		}

		public DrillingPermitRoot(ReadOnlySpan<byte> source)
		{
			//02 DA-STATUS-ROOT.
			//03 DA-PRIMARY-KEY.
			DA_STATUS_NUMBER              = StringParser.ReadAsInt64(source, 3 - 1, 7);
			DA_STATUS_SEQUENCE_NUMBER              = StringParser.ReadAsInt16(source, 10 - 1, 2);
			//03 DA-SECONDARY-KEYS.
			DA_COUNTY_CODE              = StringParser.ReadAsInt32(source, 12 - 1, 3);
			DA_LEASE_NAME              = StringParser.ReadAsString(source, 15 - 1, 32);
			DA_DISTRICT              = StringParser.ReadAsInt16(source, 47 - 1, 2);
			DA_OPERATOR_NUMBER              = StringParser.ReadAsInt64(source, 49 - 1, 10);
			//05 DA-DATE-APP-RECEIVED.
			DA_APP_RCVD_CENTURY              = StringParser.ReadAsString(source, 59 - 1, 2);
			DA_APP_RCVD_YEAR              = StringParser.ReadAsString(source, 61 - 1, 2);
			DA_APP_RCVD_MONTH              = StringParser.ReadAsString(source, 63 - 1, 2);
			DA_APP_RCVD_DAY              = StringParser.ReadAsString(source, 65 - 1, 2);
			//05 DA-DATE-APP-RCVD-RE REDEFINES
			DA_OPERATOR_NAME              = StringParser.ReadAsString(source, 67 - 1, 32);
			DA_HB1407_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 100 - 1, 1);
			DA_STATUS_OF_APP_FLAG              = StringParser.ReadAsChar(source, 101 - 1, 1);
			//03 DA-PROBLEM-FLAGS.
			DA_NOT_ENOUGH_MONEY_FLAG              = StringParser.ReadAsChar(source, 102 - 1, 1);
			DA_TOO_MUCH_MONEY_FLAG              = StringParser.ReadAsChar(source, 103 - 1, 1);
			DA_P5_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 104 - 1, 1);
			DA_P12_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 105 - 1, 1);
			DA_PLAT_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 106 - 1, 1);
			DA_W1A_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 107 - 1, 1);
			DA_OTHER_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 108 - 1, 1);
			DA_RULE37_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 109 - 1, 1);
			DA_RULE38_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 110 - 1, 1);
			DA_RULE39_PROBLEM_FLAG              = StringParser.ReadAsChar(source, 111 - 1, 1);
			DA_NO_MONEY_FLAG              = StringParser.ReadAsChar(source, 112 - 1, 1);
			DA_PERMIT              = StringParser.ReadAsInt64(source, 113 - 1, 7);
			//03 DA-ISSUE-DATE.
			DA_ISSUE_CENTURY              = StringParser.ReadAsInt16(source, 120 - 1, 2);
			DA_ISSUE_YEAR              = StringParser.ReadAsInt16(source, 122 - 1, 2);
			DA_ISSUE_MONTH              = StringParser.ReadAsInt16(source, 124 - 1, 2);
			DA_ISSUE_DAY              = StringParser.ReadAsInt16(source, 126 - 1, 2);
			//03 DA-WITHDRAWN-DATE.
			DA_WITHDRAWN_CENTURY              = StringParser.ReadAsInt16(source, 128 - 1, 2);
			DA_WITHDRAWN_YEAR              = StringParser.ReadAsInt16(source, 130 - 1, 2);
			DA_WITHDRAWN_MONTH              = StringParser.ReadAsInt16(source, 132 - 1, 2);
			DA_WITHDRAWN_DAY              = StringParser.ReadAsInt16(source, 134 - 1, 2);
			DA_WALKTHROUGH_FLAG              = StringParser.ReadAsChar(source, 136 - 1, 1);
			DA_OTHER_PROBLEM_TEXT              = StringParser.ReadAsString(source, 137 - 1, 20);
			DA_WELL_NUMBER              = StringParser.ReadAsString(source, 157 - 1, 6);
			DA_BUILT_FROM_OLD_MASTER_FLAG              = StringParser.ReadAsChar(source, 163 - 1, 1);
			DA_STATUS_RENUMBERED_TO              = StringParser.ReadAsInt64(source, 164 - 1, 9);
			DA_STATUS_RENUMBERED_FROM              = StringParser.ReadAsInt64(source, 173 - 1, 9);
			DA_APPLICATION_RETURNED_FLAG              = StringParser.ReadAsChar(source, 182 - 1, 1);
			DA_ECAP_FILING_FLAG              = StringParser.ReadAsChar(source, 183 - 1, 1);
		}
	}
}
