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
	[XmlRoot(nameof(DrillingPermitAlternateAddress))]
	[Table(nameof(DrillingPermitAlternateAddress))]
	public class DrillingPermitAlternateAddress	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-ALTERNATE-ADDRESS-SEGMENT.

		//03 DA-ALTERNATE-ADDRESS-1.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ALT_ADDRESS_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_ALT_ADDRESS_KEY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ALT_ADDRESS_LINE_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_ALT_ADDRESS_LINE_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_ALTERNATE_ADDRESS_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_ALTERNATE_ADDRESS_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitAlternateAddress()
		{
		}

		public DrillingPermitAlternateAddress(ReadOnlySpan<byte> source)
		{
			//02 DA-ALTERNATE-ADDRESS-SEGMENT.
			//03 DA-ALTERNATE-ADDRESS-1.
			DA_ALT_ADDRESS_KEY              = StringParser.ReadAsString(source, 3 - 1, 2);
			DA_ALT_ADDRESS_LINE_1              = StringParser.ReadAsString(source, 5 - 1, 0);
			DA_ALTERNATE_ADDRESS_2              = StringParser.ReadAsString(source, 5 - 1, 35);
		}
	}
}
