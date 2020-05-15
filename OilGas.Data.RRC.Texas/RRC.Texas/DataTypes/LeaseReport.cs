using System;
using System.Runtime.Serialization;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    public class LeaseReport
    {
        [DataMember]
        public string Name { get; set; }

        [DataMember]
        public string Number { get; set; }

        [DataMember]
        public string LeaseNumber { get; set; }

        [DataMember]
        public DistrictCode DistrictCode { get; set; }

        [DataMember]
        public LeaseType LeaseType { get; set; }

        public LeaseReport()
        {
        }

        public LeaseReport(string name,
                     string number,
                     string leaseNumber,
                     DistrictCode districtCode,
                     LeaseType leaseType)
        {
            Name = name;
            Number = number;
            LeaseNumber = leaseNumber;
            DistrictCode = districtCode;
            LeaseType = leaseType;
        }

        public static LeaseReport Create(WellboreQueryData    wellboreQueryData,
                                   LeaseDetailQueryData leaseDetailQueryData)
        {
            return new LeaseReport(wellboreQueryData.Columns.LeaseName,
                             wellboreQueryData.Columns.WellNo,
                             wellboreQueryData.Columns.LeaseNo,
                             new DistrictCode(wellboreQueryData.Columns.District),
                             leaseDetailQueryData.LeaseType);
        }

        public override string ToString()
        {
            return $"{Number} {DistrictCode} {LeaseType}";
        }
    }
}