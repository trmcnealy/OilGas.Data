using System;
using System.Runtime.Versioning;

namespace OilGas.Data
{
    [NonVersionable]
    public struct ApiNumber
    {
        public byte StateCode { get; }

        public ushort CountyCode { get; }

        public ushort UniqueWellIdentifier { get; }

        public byte DirectionalSidetrackCode { get; }

        public byte EventSequenceCode { get; }

        public ApiNumber(string apiNumber)
        {
            string api = apiNumber.Replace("-",
                                           "");

            if(api.Length < 10)
            {
                throw new Exception("Api number must contain 10 digits.");
            }

            StateCode = byte.Parse(api.Substring(0,
                                                 2));

            CountyCode = ushort.Parse(api.Substring(2,
                                                    3));

            UniqueWellIdentifier = ushort.Parse(api.Substring(5,
                                                              5));

            DirectionalSidetrackCode = api.Length > 10
                                           ? byte.Parse(api.Substring(10,
                                                                      2))
                                           : (byte)0;

            EventSequenceCode = api.Length > 12
                                    ? byte.Parse(api.Substring(12,
                                                               2))
                                    : (byte)0;
        }

        public static implicit operator ApiNumber(string api)
        {
            return new ApiNumber(api);
        }

        public static implicit operator ulong(ApiNumber apiNumber)
        {
            return ToUid(apiNumber);
        }

        public static ulong ToUid(ApiNumber apiNumber)
        {
            ulong dec = (ulong)apiNumber.StateCode * 1000000000000;
            dec += (ulong)apiNumber.CountyCode               * 1000000000;
            dec += (ulong)apiNumber.UniqueWellIdentifier     * 10000;
            dec += (ulong)apiNumber.DirectionalSidetrackCode * 100;
            dec += apiNumber.EventSequenceCode;

            return dec;
        }

        public override string ToString()
        {
            return $"{StateCode:D2}-{CountyCode:D3}-{UniqueWellIdentifier:D5}-{DirectionalSidetrackCode:D2}-{EventSequenceCode:D2}";
        }
    }
}