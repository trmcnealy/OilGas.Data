using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    [XmlRoot("WellProductionRecords")]
    public sealed class WellProductionRecord : IDataTable<int>, IEquatable<WellProductionRecord>
    {
        [IgnoreDataMember, XmlIgnore]
        //[Key]
        public int Id { get; set; }

        [IgnoreDataMember, XmlIgnore, ForeignKey("Records")]
        public WellProduction WellProduction { get; set; }

        [DataMember]
        [XmlElement]
        public int Month { get; set; }

        [DataMember]
        [XmlElement]
        public float MonthlyOil { get; set; }

        [DataMember]
        [XmlElement]
        public float MonthlyGas { get; set; }

        public WellProductionRecord()
        {
        }

        public WellProductionRecord(WellProduction wellProduction,
                                    int            month,
                                    float          monthlyOil,
                                    float          monthlyGas)
        {
            Id = wellProduction.Records.Count;
            WellProduction = wellProduction;
            Month          = month;
            MonthlyOil     = monthlyOil;
            MonthlyGas     = monthlyGas;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public object[] ToArray()
        {
            object[] array = new object[]
            {
                Id, Month, MonthlyOil, MonthlyGas
            };

            return array;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public IEnumerable<object> ToEnumerable()
        {
            yield return Id;
            yield return Month;
            yield return MonthlyOil;
            yield return MonthlyGas;
        }

        public bool Equals(WellProductionRecord other)
        {
            if(ReferenceEquals(null,
                               other))
            {
                return false;
            }

            if(ReferenceEquals(this,
                               other))
            {
                return true;
            }

            return Equals(WellProduction,
                          other.WellProduction)        &&
                   Month == other.Month                &&
                   MonthlyOil.Equals(other.MonthlyOil) &&
                   MonthlyGas.Equals(other.MonthlyGas);
        }

        public override bool Equals(object obj)
        {
            return ReferenceEquals(this,
                                   obj) ||
                   obj is WellProductionRecord other && Equals(other);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(WellProduction,
                                    Month,
                                    MonthlyOil,
                                    MonthlyGas);
        }

        public static bool operator ==(WellProductionRecord left,
                                       WellProductionRecord right)
        {
            return Equals(left,
                          right);
        }

        public static bool operator !=(WellProductionRecord left,
                                       WellProductionRecord right)
        {
            return !Equals(left,
                           right);
        }
    }
}