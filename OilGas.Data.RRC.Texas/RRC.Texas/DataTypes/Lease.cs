using System;
using System.Runtime.Serialization;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    public class Lease
    {
        [DataMember]
        public string Number { get; set; }

        [DataMember]
        public DistrictCode DistrictCode { get; set; }

        [DataMember]
        public LeaseType LeaseType { get; set; }

        public Lease()
        {
        }

        public Lease(string       number,
                     DistrictCode districtCode,
                     LeaseType    leaseType)
        {
            Number       = number;
            DistrictCode = districtCode;
            LeaseType    = leaseType;
        }

        public static Lease Create(WellboreQueryData    wellboreQueryData,
                                   LeaseDetailQueryData leaseDetailQueryData)
        {
            return new Lease(wellboreQueryData.Columns.Lease_No,
                             new DistrictCode(wellboreQueryData.Columns.District),
                             leaseDetailQueryData.LeaseType);
        }

        public override string ToString()
        {
            return $"{Number} {DistrictCode} {LeaseType}";
        }
    }
}