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
        public ref readonly string Value { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref _value; } }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private ScheduleType(string value) { _value = value; }

        private static readonly ScheduleType CurrentType = new ScheduleType("Y");
        public static ref readonly ScheduleType Current { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref CurrentType; } }

        private static readonly ScheduleType HistoricalType = new ScheduleType("N");
        public static ref readonly ScheduleType Historical { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref HistoricalType; } }

        private static readonly ScheduleType BothType = new ScheduleType("Both");
        public static ref readonly ScheduleType Both { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get { return ref BothType; } }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public IEnumerator<ScheduleType> GetEnumerator()
        {
            yield return Current;
            yield return Historical;
            yield return Both;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public override string ToString()
        {
            return _value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static implicit operator string(ScheduleType scheduletype)
        {
            return scheduletype._value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static implicit operator ScheduleType(string value)
        {
            return new ScheduleType(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public bool Equals(ScheduleType other) { return _value == other._value; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public override bool Equals(object obj) { return obj is ScheduleType kind && Equals(kind); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public override int GetHashCode() { return _value.GetHashCode(); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator ==(ScheduleType left, ScheduleType right) { return Equals(left, right); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator ==(ScheduleType left, string right) { return left?._value == right; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator ==(string left, ScheduleType right) { return left == right?._value; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator !=(ScheduleType left, ScheduleType right) { return !Equals(left, right); }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator !=(ScheduleType left, string right) { return left?._value != right; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool operator !=(string left, ScheduleType right) { return left != right?._value; }

    }

}

