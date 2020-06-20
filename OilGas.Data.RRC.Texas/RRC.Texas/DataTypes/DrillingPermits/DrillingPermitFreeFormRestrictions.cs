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
	[XmlRoot(nameof(DrillingPermitFreeFormRestrictions))]
	[Table(nameof(DrillingPermitFreeFormRestrictions))]
	public class DrillingPermitFreeFormRestrictions	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-FREE-RESTR-SEGMENT.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FREE_RESTR_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FREE_RESTR_KEY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FREE_RESTR_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FREE_RESTR_TYPE { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FREE_RESTR_REMARK), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FREE_RESTR_REMARK { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FREE_RESTR_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public char? DA_FREE_RESTR_FLAG { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitFreeFormRestrictions()
		{
		}

		public DrillingPermitFreeFormRestrictions(ReadOnlySpan<byte> source)
		{
			//02 DA-FREE-RESTR-SEGMENT.
			DA_FREE_RESTR_KEY              = StringParser.ReadAsInt16(source, 3 - 1, 2);
			DA_FREE_RESTR_TYPE              = StringParser.ReadAsString(source, 5 - 1, 2);
			DA_FREE_RESTR_REMARK              = StringParser.ReadAsString(source, 7 - 1, 70);
			DA_FREE_RESTR_FLAG              = StringParser.ReadAsChar(source, 77 - 1, 1);
		}
	}
}
