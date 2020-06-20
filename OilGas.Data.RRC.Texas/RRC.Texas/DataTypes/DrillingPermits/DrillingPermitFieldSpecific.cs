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
	[XmlRoot(nameof(DrillingPermitFieldSpecific))]
	[Table(nameof(DrillingPermitFieldSpecific))]
	public class DrillingPermitFieldSpecific	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-FIELD-SPECIFIC-DATA-SEGMENT.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_FIELD_DISTRICT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_LEASE_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FIELD_LEASE_NAME { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_TOTAL_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_FIELD_TOTAL_DEPTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FIELD_WELL_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FIELD_ACRES), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FIELD_ACRES { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitFieldSpecific()
		{
		}

		public DrillingPermitFieldSpecific(ReadOnlySpan<byte> source)
		{
			//02 DA-FIELD-SPECIFIC-DATA-SEGMENT.
			DA_FIELD_DISTRICT              = StringParser.ReadAsInt16(source, 3 - 1, 2);
			DA_FIELD_LEASE_NAME              = StringParser.ReadAsString(source, 5 - 1, 32);
			DA_FIELD_TOTAL_DEPTH              = StringParser.ReadAsInt64(source, 37 - 1, 5);
			DA_FIELD_WELL_NUMBER              = StringParser.ReadAsString(source, 42 - 1, 4);
			DA_FIELD_ACRES              = StringParser.ReadAsDouble(source, 2, 46 - 1, 10);
		}
	}
}
