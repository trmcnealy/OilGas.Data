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
	[XmlRoot(nameof(DrillingPermitField))]
	[Table(nameof(DrillingPermitField))]
	public class DrillingPermitField	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-FIELD-SEGMENT.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_FIELD_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_APPLICATION_WELL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_FIELD_APPLICATION_WELL_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-1995-FIELD-APPL-WELL-CODE REDEFINES

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_COMPLETION_WELL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_FIELD_COMPLETION_WELL_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_COMPLETION_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_FIELD_COMPLETION_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_TRANSFER_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_FIELD_TRANSFER_CODE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_VALIDATION_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_VALIDATION_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_VALIDATION_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_VALIDATION_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_VALIDATION_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_VALIDATION_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_VALIDATION_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_VALIDATION_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_COMPLETION_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_COMPLETION_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_COMPLETION_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_COMPLETION_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_COMPLETION_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_COMPLETION_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_COMPLETION_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_COMPLETION_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_RULE37_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_FIELD_RULE37_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_RULE38_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_FIELD_RULE38_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitField()
		{
		}

		public DrillingPermitField(ReadOnlySpan<byte> source)
		{
			//02 DA-FIELD-SEGMENT.
			DA_FIELD_NUMBER              = StringParser.ReadAsInt64(source, 3 - 1, 8);
			DA_FIELD_APPLICATION_WELL_CODE              = StringParser.ReadAsChar(source, 11 - 1, 1);
			//03 DA-1995-FIELD-APPL-WELL-CODE REDEFINES
			DA_FIELD_COMPLETION_WELL_CODE              = StringParser.ReadAsChar(source, 12 - 1, 1);
			DA_FIELD_COMPLETION_CODE              = StringParser.ReadAsChar(source, 13 - 1, 1);
			DA_FIELD_TRANSFER_CODE              = StringParser.ReadAsChar(source, 14 - 1, 1);
			DA_FIELD_VALIDATION_CENTURY              = StringParser.ReadAsInt16(source, 15 - 1, 2);
			DA_FIELD_VALIDATION_YEAR              = StringParser.ReadAsInt16(source, 17 - 1, 2);
			DA_FIELD_VALIDATION_MONTH              = StringParser.ReadAsInt16(source, 19 - 1, 2);
			DA_FIELD_VALIDATION_DAY              = StringParser.ReadAsInt16(source, 21 - 1, 2);
			DA_FIELD_COMPLETION_CENTURY              = StringParser.ReadAsInt16(source, 23 - 1, 2);
			DA_FIELD_COMPLETION_YEAR              = StringParser.ReadAsInt16(source, 25 - 1, 2);
			DA_FIELD_COMPLETION_MONTH              = StringParser.ReadAsInt16(source, 27 - 1, 2);
			DA_FIELD_COMPLETION_DAY              = StringParser.ReadAsInt16(source, 29 - 1, 2);
			DA_FIELD_RULE37_FLAG              = StringParser.ReadAsChar(source, 31 - 1, 1);
			DA_FIELD_RULE38_FLAG              = StringParser.ReadAsChar(source, 32 - 1, 1);
		}
	}
}
