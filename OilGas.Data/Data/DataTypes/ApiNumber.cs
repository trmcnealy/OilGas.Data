using System;
using System.Runtime.Versioning;

namespace OilGas.Data
{
    [NonVersionable]
    public struct ApiNumber
    {
        private readonly byte _stateCode;

        public string StateCode { get { return $"{_stateCode:D2}"; } }

        private readonly uint _countyCode;

        public string CountyCode { get { return $"{_countyCode:D3}"; } }

        private readonly uint _uniqueWellIdentifier;

        public string UniqueWellIdentifier { get { return $"{_uniqueWellIdentifier:D5}"; } }

        private readonly byte _directionalSidetrackCode;

        public string DirectionalSidetrackCode { get { return $"{_directionalSidetrackCode:D2}"; } }

        private readonly byte _eventSequenceCode;

        public string EventSequenceCode { get { return $"{_eventSequenceCode:D2}"; } }

        public ApiNumber(string apiNumber)
        {
            string api = apiNumber.Replace("-",
                                           "").Trim();

            if(api.Length < 9)
            {
                throw new Exception("Api number must contain 10 digits.");
            }

            if(api.Length == 9 || api.Length == 11 || api.Length == 13)
            {
                _stateCode = byte.Parse(api.Substring(0,
                                                      1));

                _countyCode = uint.Parse(api.Substring(1,
                                                       3));

                _uniqueWellIdentifier = uint.Parse(api.Substring(4,
                                                                 5));
            }
            else if(api.Length == 10 || api.Length == 12 || api.Length == 14)
            {
                _stateCode = byte.Parse(api.Substring(0,
                                                      2));

                _countyCode = uint.Parse(api.Substring(2,
                                                       3));

                _uniqueWellIdentifier = uint.Parse(api.Substring(5,
                                                                 5));
            }
            else
            {
                throw new Exception("Api number doesn't contain 9,10,11,12,13 or 14 digits.");
            }

            if(api.Length == 11)
            {
                _directionalSidetrackCode = byte.Parse(api.Substring(9,
                                                                     2));
            }
            else if(api.Length == 12)
            {
                _directionalSidetrackCode = byte.Parse(api.Substring(10,
                                                                     2));
            }
            else
            {
                _directionalSidetrackCode = 0;
            }

            if(api.Length == 13)
            {
                _eventSequenceCode = byte.Parse(api.Substring(11,
                                                              2));
            }
            else if(api.Length == 14)
            {
                _eventSequenceCode = byte.Parse(api.Substring(12,
                                                              2));
            }
            else
            {
                _eventSequenceCode = 0;
            }
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
            ulong dec = (ulong)apiNumber._stateCode * 1000000000000;
            dec += (ulong)apiNumber._countyCode               * 1000000000;
            dec += (ulong)apiNumber._uniqueWellIdentifier     * 10000;
            dec += (ulong)apiNumber._directionalSidetrackCode * 100;
            dec += apiNumber._eventSequenceCode;

            return dec;
        }

        public override string ToString()
        {
            return StateCode + "-" + CountyCode + "-" + UniqueWellIdentifier + "-" + DirectionalSidetrackCode + "-" + EventSequenceCode;
        }
    }
}