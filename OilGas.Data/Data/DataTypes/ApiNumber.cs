using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Runtime.Versioning;

namespace OilGas.Data
{
    [NonVersionable]
    [Serializable]
    [DataContract]
    public struct ApiNumber : IEquatable<ApiNumber>
    {
        private readonly byte _stateCode;

        [IgnoreDataMember]
        public string StateCode { get { return $"{_stateCode:D2}"; } }

        private readonly uint _countyCode;

        [IgnoreDataMember]
        public string CountyCode { get { return $"{_countyCode:D3}"; } }

        private readonly uint _uniqueWellIdentifier;

        [IgnoreDataMember]
        public string UniqueWellIdentifier { get { return $"{_uniqueWellIdentifier:D5}"; } }

        private readonly byte _directionalSidetrackCode;

        [IgnoreDataMember]
        public string DirectionalSidetrackCode { get { return $"{_directionalSidetrackCode:D2}"; } }

        private readonly byte _eventSequenceCode;

        [IgnoreDataMember]
        public string EventSequenceCode { get { return $"{_eventSequenceCode:D2}"; } }

        [IgnoreDataMember]
        public string Api { get { return StateCode + CountyCode + UniqueWellIdentifier + DirectionalSidetrackCode + EventSequenceCode; } }

        public ApiNumber(long apiNumber)
        {
            _stateCode                = (byte)(apiNumber / 1000000000000);
            _countyCode               = (uint)(apiNumber / 1000000000) % 1000;
            _uniqueWellIdentifier     = (uint)(apiNumber / 10000)      % 100000;
            _directionalSidetrackCode = (byte)(apiNumber / 100 % 100);
            _eventSequenceCode        = (byte)(apiNumber       % 100);
        }

        public ApiNumber(ulong apiNumber)
        {
            _stateCode                = (byte)(apiNumber / 1000000000000);
            _countyCode               = (uint)(apiNumber / 1000000000) % 1000;
            _uniqueWellIdentifier     = (uint)(apiNumber / 10000)      % 100000;
            _directionalSidetrackCode = (byte)(apiNumber / 100 % 100);
            _eventSequenceCode        = (byte)(apiNumber       % 100);
        }

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

        public static explicit operator long(ApiNumber apiNumber)
        {
            return (long)ToUid(apiNumber);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
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

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public override bool Equals(object obj)
        {
            return obj is ApiNumber other && Equals(other);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public bool Equals(ApiNumber other)
        {
            if(_directionalSidetrackCode == 0 && _eventSequenceCode == 0)
            {
                return _stateCode == other._stateCode && _countyCode == other._countyCode && _uniqueWellIdentifier == other._uniqueWellIdentifier;
            }
            return _stateCode                == other._stateCode                &&
                   _countyCode               == other._countyCode               &&
                   _uniqueWellIdentifier     == other._uniqueWellIdentifier     &&
                   _directionalSidetrackCode == other._directionalSidetrackCode &&
                   _eventSequenceCode        == other._eventSequenceCode;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public bool Equals(string otherString)
        {
            ApiNumber other = otherString;
            
            if(_directionalSidetrackCode == 0 && _eventSequenceCode == 0)
            {
                return _stateCode == other._stateCode && _countyCode == other._countyCode && _uniqueWellIdentifier == other._uniqueWellIdentifier;
            }

            return _stateCode                == other._stateCode                &&
                   _countyCode               == other._countyCode               &&
                   _uniqueWellIdentifier     == other._uniqueWellIdentifier     &&
                   _directionalSidetrackCode == other._directionalSidetrackCode &&
                   _eventSequenceCode        == other._eventSequenceCode;
        }

        public long GetHashCode64()
        {
            return HashCode64.Combine(_stateCode,
                                      _countyCode,
                                      _uniqueWellIdentifier,
                                      _directionalSidetrackCode,
                                      _eventSequenceCode);
        }

        public override int GetHashCode()
        {
            return (int)GetHashCode64();
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static bool operator ==(ApiNumber left,
                                       ApiNumber right)
        {
            return left.Equals(right);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static bool operator ==(string    left,
                                       ApiNumber right)
        {
            return right.Equals(left);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static bool operator ==(ApiNumber left,
                                       string    right)
        {
            return left.Equals(right);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static bool operator !=(ApiNumber left,
                                       ApiNumber right)
        {
            return !left.Equals(right);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static bool operator !=(string    left,
                                       ApiNumber right)
        {
            return !right.Equals(left);
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        public static bool operator !=(ApiNumber left,
                                       string    right)
        {
            return !left.Equals(right);
        }
    }
}