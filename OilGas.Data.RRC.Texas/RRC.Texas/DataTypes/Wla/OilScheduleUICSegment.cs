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
    [XmlRoot(nameof(OilScheduleUICSegment))]
    [Table(nameof(OilScheduleUICSegment))]
    public class OilScheduleUICSegment
    {
        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public int Id
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //02 WL-UIC-OIL-SCHEDULE-SEGMENT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UIC_CONTROL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_OIL_UIC_CONTROL_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_H_10_REQUIRED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_H_10_REQUIRED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //05 WL-OIL-UIC-STATUS-DATE.

        //10 WL-OIL-UIC-4-DIGIT-YEAR.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UIC_STATUS_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_UIC_STATUS_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UIC_STATUS_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_UIC_STATUS_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UIC_STATUS_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_UIC_STATUS_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UIC_STATUS_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_UIC_STATUS_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_H_10_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_PREV_H_10_STATUS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ROLL_SEGMENT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ROLL_SEGMENT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilScheduleUICSegment()
        {
        }

        public OilScheduleUICSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-UIC-OIL-SCHEDULE-SEGMENT.
            WL_OIL_UIC_CONTROL_NUMBER = StringParser.ReadAsDouble(source, 0, 3 - 1, 9);
            WL_OIL_H_10_REQUIRED_FLAG = StringParser.ReadAsChar(source, 12     - 1, 1);
            //05 WL-OIL-UIC-STATUS-DATE.
            //10 WL-OIL-UIC-4-DIGIT-YEAR.
            WL_OIL_UIC_STATUS_CENTURY = StringParser.ReadAsInt16(source, 13 - 1, 2);
            WL_OIL_UIC_STATUS_YEAR    = StringParser.ReadAsInt16(source, 15 - 1, 2);
            WL_OIL_UIC_STATUS_MONTH   = StringParser.ReadAsInt16(source, 17 - 1, 2);
            WL_OIL_UIC_STATUS_DAY     = StringParser.ReadAsInt16(source, 19 - 1, 2);
            WL_OIL_PREV_H_10_STATUS   = StringParser.ReadAsChar(source, 21  - 1, 1);
            WL_OIL_ROLL_SEGMENT_FLAG  = StringParser.ReadAsChar(source, 22  - 1, 1);
        }
    }
}
