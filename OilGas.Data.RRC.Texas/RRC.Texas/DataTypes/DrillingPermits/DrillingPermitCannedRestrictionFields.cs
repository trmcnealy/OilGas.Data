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
	[XmlRoot(nameof(DrillingPermitCannedRestrictionFields))]
	[Table(nameof(DrillingPermitCannedRestrictionFields))]
	public class DrillingPermitCannedRestrictionFields	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-CAN-RESTR-FLD-SEGMENT.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CAN_RESTR_FLD_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_CAN_RESTR_FLD_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitCannedRestrictionFields()
		{
		}

		public DrillingPermitCannedRestrictionFields(ReadOnlySpan<byte> source)
		{
			//02 DA-CAN-RESTR-FLD-SEGMENT.
			DA_CAN_RESTR_FLD_NUMBER              = StringParser.ReadAsInt64(source, 3 - 1, 8);
		}
	}
}
