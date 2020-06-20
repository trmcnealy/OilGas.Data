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
	[XmlRoot(nameof(DrillingPermitCheckRegister))]
	[Table(nameof(DrillingPermitCheckRegister))]
	public class DrillingPermitCheckRegister	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-CHECK-REGISTER-SEGMENT.

		//03 DA-CHECK-REGISTER-KEY.

		//05 DA-CHECK-REGISTER-DATE.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CHECK_REGISTER_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_CHECK_REGISTER_CENTURY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CHECK_REGISTER_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_CHECK_REGISTER_YEAR { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CHECK_REGISTER_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_CHECK_REGISTER_MONTH { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CHECK_REGISTER_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public short? DA_CHECK_REGISTER_DAY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_CHECK_REGISTER_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public long? DA_CHECK_REGISTER_NUMBER { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitCheckRegister()
		{
		}

		public DrillingPermitCheckRegister(ReadOnlySpan<byte> source)
		{
			//02 DA-CHECK-REGISTER-SEGMENT.
			//03 DA-CHECK-REGISTER-KEY.
			//05 DA-CHECK-REGISTER-DATE.
			DA_CHECK_REGISTER_CENTURY              = StringParser.ReadAsInt16(source, 3 - 1, 2);
			DA_CHECK_REGISTER_YEAR              = StringParser.ReadAsInt16(source, 5 - 1, 2);
			DA_CHECK_REGISTER_MONTH              = StringParser.ReadAsInt16(source, 7 - 1, 2);
			DA_CHECK_REGISTER_DAY              = StringParser.ReadAsInt16(source, 9 - 1, 2);
			DA_CHECK_REGISTER_NUMBER              = StringParser.ReadAsInt64(source, 11 - 1, 8);
		}
	}
}
