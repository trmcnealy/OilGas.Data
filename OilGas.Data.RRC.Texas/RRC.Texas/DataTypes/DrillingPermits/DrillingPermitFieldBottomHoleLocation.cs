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
	[XmlRoot(nameof(DrillingPermitFieldBottomHoleLocation))]
	[Table(nameof(DrillingPermitFieldBottomHoleLocation))]
	public class DrillingPermitFieldBottomHoleLocation	{

		[IgnoreDataMember]
		[XmlIgnore]
		[JsonIgnore]
		[Key]
		public int Id { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		//02 DA-FIELD-BHL-SEGMENT.

		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_SECTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_SECTION { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_BLOCK), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_BLOCK { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_ABSTRACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_ABSTRACT { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_SURVEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_SURVEY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_ACRES), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FLD_BHL_ACRES { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_NEAREST_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_NEAREST_WELL { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_LEASE_FEET_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FLD_BHL_LEASE_FEET_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_LEASE_DIRECTION_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_LEASE_DIRECTION_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_LEASE_FEET_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FLD_BHL_LEASE_FEET_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_LEASE_DIRECTION_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_LEASE_DIRECTION_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_SURVEY_FEET_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FLD_BHL_SURVEY_FEET_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_SURVEY_DIRECTION_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_SURVEY_DIRECTION_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_SURVEY_FEET_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FLD_BHL_SURVEY_FEET_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_SURVEY_DIRECTION_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_SURVEY_DIRECTION_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_COUNTY { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_PNTRT_DIST_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FLD_BHL_PNTRT_DIST_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_PNTRT_DIR_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_PNTRT_DIR_1 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_PNTRT_DIST_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public double? DA_FLD_BHL_PNTRT_DIST_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		[DataMember]
		[XmlElement]
		[JsonProperty(nameof(DA_FLD_BHL_PNTRT_DIR_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
		public string? DA_FLD_BHL_PNTRT_DIR_2 { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }
		public DrillingPermitFieldBottomHoleLocation()
		{
		}

		public DrillingPermitFieldBottomHoleLocation(ReadOnlySpan<byte> source)
		{
			//02 DA-FIELD-BHL-SEGMENT.
			DA_FLD_BHL_SECTION              = StringParser.ReadAsString(source, 3 - 1, 8);
			DA_FLD_BHL_BLOCK              = StringParser.ReadAsString(source, 11 - 1, 10);
			DA_FLD_BHL_ABSTRACT              = StringParser.ReadAsString(source, 21 - 1, 6);
			DA_FLD_BHL_SURVEY              = StringParser.ReadAsString(source, 27 - 1, 55);
			DA_FLD_BHL_ACRES              = StringParser.ReadAsDouble(source, 2, 82 - 1, 8);
			DA_FLD_BHL_NEAREST_WELL              = StringParser.ReadAsString(source, 90 - 1, 28);
			DA_FLD_BHL_LEASE_FEET_1              = StringParser.ReadAsDouble(source, 2, 118 - 1, 8);
			DA_FLD_BHL_LEASE_DIRECTION_1              = StringParser.ReadAsString(source, 126 - 1, 13);
			DA_FLD_BHL_LEASE_FEET_2              = StringParser.ReadAsDouble(source, 2, 139 - 1, 8);
			DA_FLD_BHL_LEASE_DIRECTION_2              = StringParser.ReadAsString(source, 147 - 1, 13);
			DA_FLD_BHL_SURVEY_FEET_1              = StringParser.ReadAsDouble(source, 2, 160 - 1, 8);
			DA_FLD_BHL_SURVEY_DIRECTION_1              = StringParser.ReadAsString(source, 168 - 1, 13);
			DA_FLD_BHL_SURVEY_FEET_2              = StringParser.ReadAsDouble(source, 2, 181 - 1, 8);
			DA_FLD_BHL_SURVEY_DIRECTION_2              = StringParser.ReadAsString(source, 189 - 1, 13);
			DA_FLD_BHL_COUNTY              = StringParser.ReadAsString(source, 202 - 1, 13);
			DA_FLD_BHL_PNTRT_DIST_1              = StringParser.ReadAsDouble(source, 2, 215 - 1, 8);
			DA_FLD_BHL_PNTRT_DIR_1              = StringParser.ReadAsString(source, 223 - 1, 13);
			DA_FLD_BHL_PNTRT_DIST_2              = StringParser.ReadAsDouble(source, 2, 236 - 1, 8);
			DA_FLD_BHL_PNTRT_DIR_2              = StringParser.ReadAsString(source, 244 - 1, 13);
		}
	}
}
