namespace OilGas.Data.RRC.Texas
{
    public struct Lease
    {
        public string Number { get; set; }

        public DistrictCode DistrictCode { get; set; }

        public LeaseType LeaseType { get; set; }

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