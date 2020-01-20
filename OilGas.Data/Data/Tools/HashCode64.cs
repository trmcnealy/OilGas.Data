using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace OilGas.Data
{
    public struct HashCode64
    {
        private static readonly ulong s_seed = GenerateGlobalSeed();
        private                 ulong _v1;
        private                 ulong _v2;
        private                 ulong _v3;
        private                 ulong _v4;
        private                 ulong _queue1;
        private                 ulong _queue2;
        private                 ulong _queue3;
        private                 ulong _length;

        internal enum NTSTATUS : uint
        {
            STATUS_SUCCESS           = 0x0,
            STATUS_NOT_FOUND         = 0xc0000225,
            STATUS_INVALID_PARAMETER = 0xc000000d,
            STATUS_NO_MEMORY         = 0xc0000017,
            STATUS_AUTH_TAG_MISMATCH = 0xc000a002,
        }

        internal const int BCRYPT_USE_SYSTEM_PREFERRED_RNG = 0x00000002;

        [DllImport("BCrypt.dll",
                   CharSet = CharSet.Unicode)]
        internal static extern unsafe NTSTATUS BCryptGenRandom(IntPtr hAlgorithm,
                                                               byte*  pbBuffer,
                                                               int    cbBuffer,
                                                               int    dwFlags);

        internal static unsafe void GetRandomBytes(byte* buffer,
                                                   int   length)
        {
            NTSTATUS status = BCryptGenRandom(IntPtr.Zero,
                                              buffer,
                                              length,
                                              BCRYPT_USE_SYSTEM_PREFERRED_RNG);

            if(status != NTSTATUS.STATUS_SUCCESS)
            {
                if(status == NTSTATUS.STATUS_NO_MEMORY)
                {
                    throw new OutOfMemoryException();
                }

                throw new InvalidOperationException();
            }
        }

        private static unsafe ulong GenerateGlobalSeed()
        {
            ulong num;

            GetRandomBytes((byte*)&num,
                           4);

            return num;
        }

        public static long Combine<T1>(T1 value1)
        {
            return (long)MixFinal(QueueRound(MixEmptyState() + 4U,
                                             value1 != null ? (ulong)value1.GetHashCode() : 0U));
        }

        public static long Combine<T1, T2>(T1 value1,
                                           T2 value2)
        {
            return (long)MixFinal(QueueRound(QueueRound(MixEmptyState() + 8U,
                                                        value1 != null ? (ulong)value1.GetHashCode() : 0U),
                                             value2 != null ? (ulong)value2.GetHashCode() : 0U));
        }

        public static long Combine<T1, T2, T3>(T1 value1,
                                               T2 value2,
                                               T3 value3)
        {
            return (long)MixFinal(QueueRound(QueueRound(QueueRound(MixEmptyState() + 12U,
                                                                   value1 != null ? (ulong)value1.GetHashCode() : 0U),
                                                        value2 != null ? (ulong)value2.GetHashCode() : 0U),
                                             value3 != null ? (ulong)value3.GetHashCode() : 0U));
        }

        public static long Combine<T1, T2, T3, T4>(T1 value1,
                                                   T2 value2,
                                                   T3 value3,
                                                   T4 value4)
        {
            ulong input1 = value1 != null ? (ulong)value1.GetHashCode() : 0U;
            ulong input2 = value2 != null ? (ulong)value2.GetHashCode() : 0U;
            ulong input3 = value3 != null ? (ulong)value3.GetHashCode() : 0U;
            ulong input4 = value4 != null ? (ulong)value4.GetHashCode() : 0U;
            ulong v1_1;
            ulong v2;
            ulong v3;
            ulong v4;

            Initialize(out v1_1,
                       out v2,
                       out v3,
                       out v4);

            ulong v1_2 = Round(v1_1,
                               input1);

            v2 = Round(v2,
                       input2);

            v3 = Round(v3,
                       input3);

            v4 = Round(v4,
                       input4);

            return (long)MixFinal(MixState(v1_2,
                                           v2,
                                           v3,
                                           v4) +
                                  16U);
        }

        public static long Combine<T1, T2, T3, T4, T5>(T1 value1,
                                                       T2 value2,
                                                       T3 value3,
                                                       T4 value4,
                                                       T5 value5)
        {
            ulong input1      = value1 != null ? (ulong)value1.GetHashCode() : 0U;
            ulong input2      = value2 != null ? (ulong)value2.GetHashCode() : 0U;
            ulong input3      = value3 != null ? (ulong)value3.GetHashCode() : 0U;
            ulong input4      = value4 != null ? (ulong)value4.GetHashCode() : 0U;
            ulong queuedValue = value5 != null ? (ulong)value5.GetHashCode() : 0U;
            ulong v1_1;
            ulong v2;
            ulong v3;
            ulong v4;

            Initialize(out v1_1,
                       out v2,
                       out v3,
                       out v4);

            ulong v1_2 = Round(v1_1,
                               input1);

            v2 = Round(v2,
                       input2);

            v3 = Round(v3,
                       input3);

            v4 = Round(v4,
                       input4);

            return (long)MixFinal(QueueRound(MixState(v1_2,
                                                      v2,
                                                      v3,
                                                      v4) +
                                             20U,
                                             queuedValue));
        }

        public static long Combine<T1, T2, T3, T4, T5, T6>(T1 value1,
                                                           T2 value2,
                                                           T3 value3,
                                                           T4 value4,
                                                           T5 value5,
                                                           T6 value6)
        {
            ulong input1       = value1 != null ? (ulong)value1.GetHashCode() : 0U;
            ulong input2       = value2 != null ? (ulong)value2.GetHashCode() : 0U;
            ulong input3       = value3 != null ? (ulong)value3.GetHashCode() : 0U;
            ulong input4       = value4 != null ? (ulong)value4.GetHashCode() : 0U;
            ulong queuedValue1 = value5 != null ? (ulong)value5.GetHashCode() : 0U;
            ulong queuedValue2 = value6 != null ? (ulong)value6.GetHashCode() : 0U;
            ulong v1_1;
            ulong v2;
            ulong v3;
            ulong v4;

            Initialize(out v1_1,
                       out v2,
                       out v3,
                       out v4);

            ulong v1_2 = Round(v1_1,
                               input1);

            v2 = Round(v2,
                       input2);

            v3 = Round(v3,
                       input3);

            v4 = Round(v4,
                       input4);

            return (long)MixFinal(QueueRound(QueueRound(MixState(v1_2,
                                                                 v2,
                                                                 v3,
                                                                 v4) +
                                                        24U,
                                                        queuedValue1),
                                             queuedValue2));
        }

        public static long Combine<T1, T2, T3, T4, T5, T6, T7>(T1 value1,
                                                               T2 value2,
                                                               T3 value3,
                                                               T4 value4,
                                                               T5 value5,
                                                               T6 value6,
                                                               T7 value7)
        {
            ulong input1       = value1 != null ? (ulong)value1.GetHashCode() : 0U;
            ulong input2       = value2 != null ? (ulong)value2.GetHashCode() : 0U;
            ulong input3       = value3 != null ? (ulong)value3.GetHashCode() : 0U;
            ulong input4       = value4 != null ? (ulong)value4.GetHashCode() : 0U;
            ulong queuedValue1 = value5 != null ? (ulong)value5.GetHashCode() : 0U;
            ulong queuedValue2 = value6 != null ? (ulong)value6.GetHashCode() : 0U;
            ulong queuedValue3 = value7 != null ? (ulong)value7.GetHashCode() : 0U;
            ulong v1_1;
            ulong v2;
            ulong v3;
            ulong v4;

            Initialize(out v1_1,
                       out v2,
                       out v3,
                       out v4);

            ulong v1_2 = Round(v1_1,
                               input1);

            v2 = Round(v2,
                       input2);

            v3 = Round(v3,
                       input3);

            v4 = Round(v4,
                       input4);

            return (long)MixFinal(QueueRound(QueueRound(QueueRound(MixState(v1_2,
                                                                            v2,
                                                                            v3,
                                                                            v4) +
                                                                   28U,
                                                                   queuedValue1),
                                                        queuedValue2),
                                             queuedValue3));
        }

        public static long Combine<T1, T2, T3, T4, T5, T6, T7, T8>(T1 value1,
                                                                   T2 value2,
                                                                   T3 value3,
                                                                   T4 value4,
                                                                   T5 value5,
                                                                   T6 value6,
                                                                   T7 value7,
                                                                   T8 value8)
        {
            ulong input1 = value1 != null ? (ulong)value1.GetHashCode() : 0U;
            ulong input2 = value2 != null ? (ulong)value2.GetHashCode() : 0U;
            ulong input3 = value3 != null ? (ulong)value3.GetHashCode() : 0U;
            ulong input4 = value4 != null ? (ulong)value4.GetHashCode() : 0U;
            ulong input5 = value5 != null ? (ulong)value5.GetHashCode() : 0U;
            ulong input6 = value6 != null ? (ulong)value6.GetHashCode() : 0U;
            ulong input7 = value7 != null ? (ulong)value7.GetHashCode() : 0U;
            ulong input8 = value8 != null ? (ulong)value8.GetHashCode() : 0U;
            ulong v1_1;
            ulong v2;
            ulong v3;
            ulong v4;

            Initialize(out v1_1,
                       out v2,
                       out v3,
                       out v4);

            ulong hash = Round(v1_1,
                               input1);

            v2 = Round(v2,
                       input2);

            v3 = Round(v3,
                       input3);

            v4 = Round(v4,
                       input4);

            ulong v1_2 = Round(hash,
                               input5);

            v2 = Round(v2,
                       input6);

            v3 = Round(v3,
                       input7);

            v4 = Round(v4,
                       input8);

            return (long)MixFinal(MixState(v1_2,
                                           v2,
                                           v3,
                                           v4) +
                                  32U);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void Initialize(out ulong v1,
                                       out ulong v2,
                                       out ulong v3,
                                       out ulong v4)
        {
            v1 = (ulong)((long)s_seed - 1640531535 - 2048144777);
            v2 = s_seed + 2246822519U;
            v3 = s_seed;
            v4 = s_seed - 2654435761U;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static ulong Round(ulong hash,
                                   ulong input)
        {
            return BitOperations.RotateLeft(hash + input * 2246822519U,
                                            13) *
                   2654435761U;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static ulong QueueRound(ulong hash,
                                        ulong queuedValue)
        {
            return BitOperations.RotateLeft(hash + queuedValue * 3266489917U,
                                            17) *
                   668265263U;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static ulong MixState(ulong v1,
                                      ulong v2,
                                      ulong v3,
                                      ulong v4)
        {
            return BitOperations.RotateLeft(v1,
                                            1) +
                   BitOperations.RotateLeft(v2,
                                            7) +
                   BitOperations.RotateLeft(v3,
                                            12) +
                   BitOperations.RotateLeft(v4,
                                            18);
        }

        private static ulong MixEmptyState()
        {
            return s_seed + 374761393U;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static ulong MixFinal(ulong hash)
        {
            hash ^= hash >> 15;
            hash *= 2246822519U;
            hash ^= hash >> 13;
            hash *= 3266489917U;
            hash ^= hash >> 16;

            return hash;
        }

        public void Add<T>(T value)
        {
            Add(value != null ? value.GetHashCode() : 0);
        }

        public void Add<T>(T                    value,
                           IEqualityComparer<T> comparer)
        {
            Add(comparer != null ? comparer.GetHashCode(value) : value != null ? value.GetHashCode() : 0);
        }

        private void Add(long value)
        {
            ulong input = (ulong)value;
            ulong num   = _length++;

            switch(num % 4U)
            {
                case 0:
                    _queue1 = input;

                    break;
                case 1:
                    _queue2 = input;

                    break;
                case 2:
                    _queue3 = input;

                    break;
                default:
                    if(num == 3U)
                    {
                        Initialize(out _v1,
                                   out _v2,
                                   out _v3,
                                   out _v4);
                    }

                    _v1 = Round(_v1,
                                _queue1);

                    _v2 = Round(_v2,
                                _queue2);

                    _v3 = Round(_v3,
                                _queue3);

                    _v4 = Round(_v4,
                                input);

                    break;
            }
        }

        public long ToHashCode()
        {
            ulong length = _length;
            ulong num    = length % 4U;

            ulong hash = (length < 4U
                              ? MixEmptyState()
                              : MixState(_v1,
                                         _v2,
                                         _v3,
                                         _v4)) +
                         length * 4U;

            if(num > 0U)
            {
                hash = QueueRound(hash,
                                  _queue1);

                if(num > 1U)
                {
                    hash = QueueRound(hash,
                                      _queue2);

                    if(num > 2U)
                    {
                        hash = QueueRound(hash,
                                          _queue3);
                    }
                }
            }

            return (long)MixFinal(hash);
        }
    }
}