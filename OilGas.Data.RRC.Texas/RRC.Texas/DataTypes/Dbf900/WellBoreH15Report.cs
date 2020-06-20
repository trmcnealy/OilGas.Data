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
    [XmlRoot(nameof(WellBoreH15Report))]
    [Table(nameof(WellBoreH15Report))]
    public class WellBoreH15Report
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
        //02 WELL-BORE-H15-RECORD.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_DATE_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_H15_DATE_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_STATUS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_OPERATOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_H15_OPERATOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-H15-NEXT-TEST-DUE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_NEXT_TEST_CCYY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_NEXT_TEST_CCYY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_NEXT_TEST_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_NEXT_TEST_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_FIELD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_H15_FIELD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_HIST_WELLBORE_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_HIST_WELLBORE_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-H15-HIST-WELL-CCYYMMDD.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_HIST_WELL_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_HIST_WELL_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-H15-HIST-WELL-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_HIST_WELL_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_HIST_WELL_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_HIST_WELL_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_HIST_WELL_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_HIST_WELL_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_HIST_WELL_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_W1X_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_W1X_WELL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_OIL_GAS_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_OIL_GAS_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_LEASE_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_H15_LEASE_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_WELL_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_H15_WELL_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_GASID_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_H15_GASID_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-H15-TEST-DATE.

        //05 WB-H15-TEST-YEAR.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_TEST_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_TEST_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_TEST_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_TEST_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_TEST_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_TEST_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_TEST_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_TEST_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_BASE_USABLE_WATER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_H15_BASE_USABLE_WATER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_TYPE_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_TYPE_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_TOP_OF_FLUID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_H15_TOP_OF_FLUID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_FLUID_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_FLUID_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_MECH_INTEG_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_MECH_INTEG_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_MECH_TEST_REASON_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_MECH_TEST_REASON_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_ALTERNATE_TEST_PERIOD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_ALTERNATE_TEST_PERIOD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_OTHER_MIT_TEST_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_H15_OTHER_MIT_TEST_TYPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-H15-STATUS-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_STATUS_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_STATUS_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_STATUS_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_STATUS_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_STATUS_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_STATUS_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_STATUS_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_STATUS_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_NO_DATE_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_NO_DATE_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_RECORD_FROM_EDI_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_RECORD_FROM_EDI_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-H15-KEYED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_KEYED_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_KEYED_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_KEYED_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_KEYED_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_KEYED_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_KEYED_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_KEYED_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_KEYED_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-H15-CHANGED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_CHANGED_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_CHANGED_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_CHANGED_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_CHANGED_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_CHANGED_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_CHANGED_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_CHANGED_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_H15_CHANGED_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_PREVIOUS_STATUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_PREVIOUS_STATUS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_UIC_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_UIC_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_2YRS_APPROVED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_2YRS_APPROVED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_MAIL_HOLD_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_MAIL_HOLD_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_10YR_INACTIVE_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_10YR_INACTIVE_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_W3X_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_H15_W3X_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreH15Remarks>? WellBoreH15Remarks
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreH15Remarks>();

        public WellBoreH15Report()
        {
        }

        public WellBoreH15Report(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-H15-RECORD.
            WB_H15_DATE_KEY = StringParser.ReadAsInt64(source, 3  - 1, 8);
            WB_H15_STATUS   = StringParser.ReadAsChar(source, 11  - 1, 1);
            WB_H15_OPERATOR = StringParser.ReadAsInt64(source, 12 - 1, 6);
            //03 WB-H15-NEXT-TEST-DUE-DATE.
            WB_NEXT_TEST_CCYY         = StringParser.ReadAsInt32(source, 18 - 1, 4);
            WB_NEXT_TEST_MM           = StringParser.ReadAsInt16(source, 22 - 1, 2);
            WB_H15_DISTRICT           = StringParser.ReadAsInt16(source, 24 - 1, 2);
            WB_H15_FIELD              = StringParser.ReadAsInt64(source, 26 - 1, 8);
            WB_H15_HIST_WELLBORE_FLAG = StringParser.ReadAsChar(source, 34  - 1, 1);
            //03 WB-H15-HIST-WELL-CCYYMMDD.
            WB_H15_HIST_WELL_CC = StringParser.ReadAsInt16(source, 35 - 1, 2);
            //05 WB-H15-HIST-WELL-DATE.
            WB_H15_HIST_WELL_YY = StringParser.ReadAsInt16(source, 37  - 1, 2);
            WB_H15_HIST_WELL_MM = StringParser.ReadAsInt16(source, 39  - 1, 2);
            WB_H15_HIST_WELL_DD = StringParser.ReadAsInt16(source, 41  - 1, 2);
            WB_H15_W1X_WELL     = StringParser.ReadAsChar(source, 43   - 1, 1);
            WB_H15_OIL_GAS_CODE = StringParser.ReadAsChar(source, 44   - 1, 1);
            WB_H15_LEASE_NBR    = StringParser.ReadAsInt64(source, 45  - 1, 5);
            WB_H15_WELL_NBR     = StringParser.ReadAsString(source, 50 - 1, 6);
            WB_H15_GASID_NBR    = StringParser.ReadAsInt64(source, 56  - 1, 6);
            //03 WB-H15-TEST-DATE.
            //05 WB-H15-TEST-YEAR.
            WB_H15_TEST_CC               = StringParser.ReadAsInt16(source, 62  - 1, 2);
            WB_H15_TEST_YY               = StringParser.ReadAsInt16(source, 64  - 1, 2);
            WB_H15_TEST_MM               = StringParser.ReadAsInt16(source, 66  - 1, 2);
            WB_H15_TEST_DD               = StringParser.ReadAsInt16(source, 68  - 1, 2);
            WB_H15_BASE_USABLE_WATER     = StringParser.ReadAsInt64(source, 70  - 1, 6);
            WB_H15_TYPE_TEST_FLAG        = StringParser.ReadAsChar(source, 76   - 1, 1);
            WB_H15_TOP_OF_FLUID          = StringParser.ReadAsInt64(source, 77  - 1, 6);
            WB_H15_FLUID_TEST_FLAG       = StringParser.ReadAsChar(source, 83   - 1, 1);
            WB_H15_MECH_INTEG_TEST_FLAG  = StringParser.ReadAsChar(source, 84   - 1, 1);
            WB_H15_MECH_TEST_REASON_FLAG = StringParser.ReadAsChar(source, 85   - 1, 1);
            WB_H15_ALTERNATE_TEST_PERIOD = StringParser.ReadAsInt16(source, 86  - 1, 2);
            WB_H15_OTHER_MIT_TEST_TYPE   = StringParser.ReadAsString(source, 88 - 1, 20);
            //03 WB-H15-STATUS-DATE.
            WB_H15_STATUS_CC            = StringParser.ReadAsInt16(source, 108 - 1, 2);
            WB_H15_STATUS_YY            = StringParser.ReadAsInt16(source, 110 - 1, 2);
            WB_H15_STATUS_MM            = StringParser.ReadAsInt16(source, 112 - 1, 2);
            WB_H15_STATUS_DD            = StringParser.ReadAsInt16(source, 114 - 1, 2);
            WB_H15_NO_DATE_WELL_FLAG    = StringParser.ReadAsChar(source, 116  - 1, 1);
            WB_H15_RECORD_FROM_EDI_FLAG = StringParser.ReadAsChar(source, 117  - 1, 1);
            //03 WB-H15-KEYED-DATE.
            WB_H15_KEYED_CC = StringParser.ReadAsInt16(source, 118 - 1, 2);
            WB_H15_KEYED_YY = StringParser.ReadAsInt16(source, 120 - 1, 2);
            WB_H15_KEYED_MM = StringParser.ReadAsInt16(source, 122 - 1, 2);
            WB_H15_KEYED_DD = StringParser.ReadAsInt16(source, 124 - 1, 2);
            //03 WB-H15-CHANGED-DATE.
            WB_H15_CHANGED_CC         = StringParser.ReadAsInt16(source, 126 - 1, 2);
            WB_H15_CHANGED_YY         = StringParser.ReadAsInt16(source, 128 - 1, 2);
            WB_H15_CHANGED_MM         = StringParser.ReadAsInt16(source, 130 - 1, 2);
            WB_H15_CHANGED_DD         = StringParser.ReadAsInt16(source, 132 - 1, 2);
            WB_H15_PREVIOUS_STATUS    = StringParser.ReadAsChar(source, 134  - 1, 1);
            WB_H15_UIC_TEST_FLAG      = StringParser.ReadAsChar(source, 135  - 1, 1);
            WB_H15_2YRS_APPROVED_FLAG = StringParser.ReadAsChar(source, 136  - 1, 1);
            WB_H15_MAIL_HOLD_FLAG     = StringParser.ReadAsChar(source, 137  - 1, 1);
            WB_H15_10YR_INACTIVE_FLAG = StringParser.ReadAsChar(source, 138  - 1, 1);
            WB_H15_W3X_WELL_FLAG      = StringParser.ReadAsChar(source, 139  - 1, 1);
        }
    }
}
