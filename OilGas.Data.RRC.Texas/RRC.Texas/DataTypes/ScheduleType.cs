// ReSharper disable ConvertToAutoProperty
// ReSharper disable InconsistentNaming

using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace OilGas.Data.RRC.Texas
{
    [System.Runtime.Versioning.NonVersionable]
    public sealed class ScheduleType : IEnumerable<ScheduleType>
    {
        private readonly string _value;

        public string Value
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get { return _value; }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ScheduleType(string value)
        {
            _value = value;
        }

        private static readonly ScheduleType CurrentType = new ScheduleType("Y");

        public static ScheduleType Current
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get { return CurrentType; }
        }

        private static readonly ScheduleType HistoricalType = new ScheduleType("N");

        public static ScheduleType Historical
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get { return HistoricalType; }
        }

        private static readonly ScheduleType BothType = new ScheduleType("Both");

        public static ScheduleType Both
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get { return BothType; }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public IEnumerator<ScheduleType> GetEnumerator()
        {
            yield return Current;
            yield return Historical;
            yield return Both;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override string ToString()
        {
            return _value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static implicit operator string(ScheduleType scheduletype)
        {
            return scheduletype.Value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static implicit operator ScheduleType(string value)
        {
            return new ScheduleType(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Equals(ScheduleType other)
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

            return Value == other.Value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override bool Equals(object obj)
        {
            if(ReferenceEquals(null,
                               obj))
            {
                return false;
            }

            if(ReferenceEquals(this,
                               obj))
            {
                return true;
            }

            return obj is ScheduleType kind && Equals(kind);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator ==(ScheduleType left,
                                       ScheduleType right)
        {
            return Equals(left,
                          right);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator ==(ScheduleType left,
                                       string       right)
        {
            return left?.Value == right;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator ==(string       left,
                                       ScheduleType right)
        {
            return left == right?.Value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator !=(ScheduleType left,
                                       ScheduleType right)
        {
            return !Equals(left,
                           right);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator !=(ScheduleType left,
                                       string       right)
        {
            return left?.Value != right;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool operator !=(string       left,
                                       ScheduleType right)
        {
            return left != right?.Value;
        }
    }
}