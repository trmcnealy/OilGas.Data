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
	[XmlRoot(nameof(DrillingPermitRemarks))]
	[Table(nameof(DrillingPermitRemarks))]
	public class DrillingPermitRemarks	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-REMARKS-SEGMENT.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_REMARK_SEQUENCE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public int? DA_REMARK_SEQUENCE_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//03 DA-REMARK-FILE-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_REMARK_FILE_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_REMARK_FILE_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_REMARK_FILE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_REMARK_FILE_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_REMARK_FILE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_REMARK_FILE_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_REMARK_FILE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_REMARK_FILE_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_REMARK_LINE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_REMARK_LINE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitRemarks()
		{
		}

		public DrillingPermitRemarks(ReadOnlySpan<byte> source)
		{
			//02 DA-REMARKS-SEGMENT.
			DA_REMARK_SEQUENCE_NUMBER              = StringParser.ReadAsInt32(source, 3 - 1, 3);
			//03 DA-REMARK-FILE-DATE.
			DA_REMARK_FILE_CENTURY              = StringParser.ReadAsInt16(source, 6 - 1, 2);
			DA_REMARK_FILE_YEAR              = StringParser.ReadAsInt16(source, 8 - 1, 2);
			DA_REMARK_FILE_MONTH              = StringParser.ReadAsInt16(source, 10 - 1, 2);
			DA_REMARK_FILE_DAY              = StringParser.ReadAsInt16(source, 12 - 1, 2);
			DA_REMARK_LINE              = StringParser.ReadAsString(source, 14 - 1, 70);
		}
	}
}
