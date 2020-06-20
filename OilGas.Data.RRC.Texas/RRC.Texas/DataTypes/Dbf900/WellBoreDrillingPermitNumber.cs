#nullable enable
using System;
using System.Collections.Generic;
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
    [XmlRoot(nameof(WellBoreDrillingPermitNumber))]
    [Table(nameof(WellBoreDrillingPermitNumber))]
    public class WellBoreDrillingPermitNumber
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
        //02 WELL-BORE-DRLG-PMT-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PERMIT_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PERMIT_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreWellId>? WellBoreWellId
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreWellId>();

        public WellBoreDrillingPermitNumber()
        {
        }

        public WellBoreDrillingPermitNumber(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-DRLG-PMT-SEG.
            WB_PERMIT_NUMBER = StringParser.ReadAsInt64(source, 3 - 1, 6);
        }
    }
}
