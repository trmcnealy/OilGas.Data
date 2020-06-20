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
	[XmlRoot(nameof(DrillingPermitFreeFormRestrictionFields))]
	[Table(nameof(DrillingPermitFreeFormRestrictionFields))]
	public class DrillingPermitFreeFormRestrictionFields	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-FREE-RESTR-FLD-SEGMENT.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FREE_RESTR_FLD_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_FREE_RESTR_FLD_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitFreeFormRestrictionFields()
		{
		}

		public DrillingPermitFreeFormRestrictionFields(ReadOnlySpan<byte> source)
		{
			//02 DA-FREE-RESTR-FLD-SEGMENT.
			DA_FREE_RESTR_FLD_NUMBER              = StringParser.ReadAsInt64(source, 3 - 1, 8);
		}
	}
}
