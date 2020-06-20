#nullable enable

using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;

using AngleSharp.Text;

using Engineering.DataSource;

namespace OilGas.Data.RRC.Texas
{
    public static class StringParser
    {
        #region ref index

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static byte? ReadAsByte(ReadOnlySpan<char> source,
                                       ref int            position,
                                       int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            try
            {
                return byte.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static sbyte? ReadAsSByte(ReadOnlySpan<char> source,
                                         ref int            position,
                                         int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            try
            {
                return sbyte.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool? ReadAsBoolean(ReadOnlySpan<char> source,
                                          ref int            position,
                                          int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            try
            {
                return bool.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static char? ReadAsChar(ReadOnlySpan<char> source,
                                       ref int            position,
                                       int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(value[0] == char.MinValue)
            {
                Debugger.Break();
            }

            try
            {
                return value[0];
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static short? ReadAsInt16(ReadOnlySpan<char> source,
                                         ref int            position,
                                         int                count = 2)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!short.TryParse(value, out short result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static ushort? ReadAsUInt16(ReadOnlySpan<char> source,
                                           ref int            position,
                                           int                count = 2)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!ushort.TryParse(value, out ushort result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static int? ReadAsInt32(ReadOnlySpan<char> source,
                                       ref int            position,
                                       int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!int.TryParse(value, out int result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static uint? ReadAsUInt32(ReadOnlySpan<char> source,
                                         ref int            position,
                                         int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!uint.TryParse(value, out uint result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static long? ReadAsInt64(ReadOnlySpan<char> source,
                                        ref int            position,
                                        int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!long.TryParse(value, out long result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static ulong? ReadAsUInt64(ReadOnlySpan<char> source,
                                          ref int            position,
                                          int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!ulong.TryParse(value, out ulong result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static float? ReadAsSingle(ReadOnlySpan<char> source,
                                          ref int            position,
                                          int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!float.TryParse(value, out float result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double? ReadAsDouble(ReadOnlySpan<char> source,
                                           ref int            position,
                                           int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            if(!double.TryParse(value, out double result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static string? ReadAsString(ReadOnlySpan<char> source,
                                           ref int            position,
                                           int                count)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            position += count;

            for(int i = 0; i < value.Length; ++i)
            {
                if(value[i] == char.MinValue)
                {
                    Debugger.Break();
                }
            }

            return new string(value);
        }

        #endregion

        #region index

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static byte? ReadAsByte(ReadOnlySpan<char> source,
                                       int                position,
                                       int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            try
            {
                return byte.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static sbyte? ReadAsSByte(ReadOnlySpan<char> source,
                                         int                position,
                                         int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            try
            {
                return sbyte.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool? ReadAsBoolean(ReadOnlySpan<char> source,
                                          int                position,
                                          int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            try
            {
                return bool.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static char? ReadAsChar(ReadOnlySpan<char> source,
                                       int                position,
                                       int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(value[0] == char.MinValue)
            {
                Debugger.Break();
            }

            try
            {
                return value[0];
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static short? ReadAsInt16(ReadOnlySpan<char> source,
                                         int                position,
                                         int                count = 2)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!short.TryParse(value, out short result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static ushort? ReadAsUInt16(ReadOnlySpan<char> source,
                                           int                position,
                                           int                count = 2)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!ushort.TryParse(value, out ushort result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static int? ReadAsInt32(ReadOnlySpan<char> source,
                                       int                position,
                                       int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!int.TryParse(value, out int result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static uint? ReadAsUInt32(ReadOnlySpan<char> source,
                                         int                position,
                                         int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!uint.TryParse(value, out uint result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static long? ReadAsInt64(ReadOnlySpan<char> source,
                                        int                position,
                                        int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!long.TryParse(value, out long result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static ulong? ReadAsUInt64(ReadOnlySpan<char> source,
                                          int                position,
                                          int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!ulong.TryParse(value, out ulong result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static float? ReadAsSingle(ReadOnlySpan<char> source,
                                          int                position,
                                          int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!float.TryParse(value, out float result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double? ReadAsDouble(ReadOnlySpan<char> source,
                                           int                position,
                                           int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!double.TryParse(value, out double result))
            {
                return null;
            }

            return result;
        }

        private static readonly string NullString = new string(new[] {char.MinValue});

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static string? ReadAsString(ReadOnlySpan<char> source,
                                           int                position,
                                           int                count)
        {
            int index = position;

            ReadOnlySpan<char> value = source.Slice(index, count).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            return new string(value);
        }

        #endregion

        #region index

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static byte? ReadAsByte(ReadOnlySpan<byte> source,
                                       int                position,
                                       int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            try
            {
                return byte.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static sbyte? ReadAsSByte(ReadOnlySpan<byte> source,
                                         int                position,
                                         int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            try
            {
                return sbyte.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static bool? ReadAsBoolean(ReadOnlySpan<byte> source,
                                          int                position,
                                          int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            try
            {
                return bool.Parse(value);
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static char? ReadAsChar(ReadOnlySpan<byte> source,
                                       int                position,
                                       int                count = 1)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0 || !value[0].IsAlphanumericAscii())
            {
                return null;
            }

            if(value[0] == char.MinValue)
            {
                Debugger.Break();
            }

            try
            {
                return value[0];
            }
            catch(Exception)
            {
                // ignored
            }

            return null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static short? ReadAsInt16(ReadOnlySpan<byte> source,
                                         int                position,
                                         int                count = 2)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!short.TryParse(value, out short result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static ushort? ReadAsUInt16(ReadOnlySpan<byte> source,
                                           int                position,
                                           int                count = 2)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!ushort.TryParse(value, out ushort result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static int? ReadAsInt32(ReadOnlySpan<byte> source,
                                       int                position,
                                       int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!int.TryParse(value, out int result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static uint? ReadAsUInt32(ReadOnlySpan<byte> source,
                                         int                position,
                                         int                count = 4)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!uint.TryParse(value, out uint result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static long? ReadAsInt64(ReadOnlySpan<byte> source,
                                        int                position,
                                        int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!long.TryParse(value, out long result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static ulong? ReadAsUInt64(ReadOnlySpan<byte> source,
                                          int                position,
                                          int                count = 8)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(!ulong.TryParse(value, out ulong result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static float? ReadAsSingle(ReadOnlySpan<byte> source,
                                          int                decimalPlace,
                                          int                position,
                                          int                count = 4)
        {
            int index = position;

            string value = Utilities.ToAscii(source.Slice(index, count).TrimNull());

            if(value.Length == 0)
            {
                return null;
            }

            if(count == decimalPlace)
            {
                value = "0." + value;
            }
            else if(value.Contains("{"))
            {
                value = value.Replace("{", ".0");
            }
            else if(char.IsLetter(value[^1]))
            {
                value = value.Substring(0, value.Length - 1) + ".0";
            }
            else if(value.Length < decimalPlace)
            {
                value += ".0";
            }
            else
            {
                value = value.Substring(0, value.Length - decimalPlace) + '.' + value.Substring(value.Length - decimalPlace, decimalPlace);
            }

            if(!float.TryParse(value, out float result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double? ReadAsDouble(ReadOnlySpan<byte> source,
                                           int                decimalPlace,
                                           int                position,
                                           int                count = 8)
        {
            int index = position;

            string value = Utilities.ToAscii(source.Slice(index, count).TrimNull());

            if(value.Length == 0)
            {
                return null;
            }

            if(count == decimalPlace)
            {
                value = "0." + value;
            }
            else if(value.Contains("{"))
            {
                value = value.Replace("{", ".0");
            }
            else if(value.Length < decimalPlace)
            {
                value += ".0";
            }
            else
            {
                value = value.Substring(0, value.Length - decimalPlace) + '.' + value.Substring(value.Length - decimalPlace, decimalPlace);
            }

            if(!double.TryParse(value, out double result))
            {
                return null;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static string? ReadAsString(ReadOnlySpan<byte> source,
                                           int                position,
                                           int                count)
        {
            int index = position;

            ReadOnlySpan<char> value = Utilities.ToAscii(source.Slice(index, count)).TrimWhiteOrNull();

            if(value.Length == 0)
            {
                return null;
            }

            if(value.ToArray().Any(c => c == char.MinValue))
            {
                return new string(value).Replace(char.MinValue, ' ');
            }

            return new string(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static float? ReadAsPackedSingle(ReadOnlySpan<byte> source,
                                                int                decimalPlace,
                                                int                position,
                                                int                count)
        {
            int index = position;

            ReadOnlySpan<byte> value = source.Slice(index, count);

            if(value.Length == 0)
            {
                return null;
            }

            return UnpackDataSingle(value, decimalPlace);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double? ReadAsPackedDouble(ReadOnlySpan<byte> source,
                                                 int                decimalPlace,
                                                 int                position,
                                                 int                count)
        {
            int index = position;

            ReadOnlySpan<byte> value = source.Slice(index, count);

            if(value.Length == 0)
            {
                return null;
            }

            return UnpackDataDouble(value, decimalPlace);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static int? ReadAsPackedInt32(ReadOnlySpan<byte> source,
                                              int                position,
                                              int                count)
        {
            int index = position;

            ReadOnlySpan<byte> value = source.Slice(index, count);

            if(value.Length == 0)
            {
                return null;
            }

            return UnpackDataInt32(value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static long? ReadAsPackedInt64(ReadOnlySpan<byte> source,
                                              int                position,
                                              int                count)
        {
            int index = position;

            ReadOnlySpan<byte> value = source.Slice(index, count);

            if(value.Length == 0)
            {
                return null;
            }

            return UnpackDataInt64(value);
        }

        #endregion

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static int? UnpackDataInt32(ReadOnlySpan<byte> bytes)
        {
            string str = "";

            for(int i = 0; i < bytes.Length; ++i)
            {
                str += bytes[i].ToString("x2");
            }

            char lastChar = str[^1];

            if(lastChar.IsLetter())
            {
                str = str.Substring(0, str.Length - 1);
            }

            if(str.All(c => c == '9'))
            {
                return 0;
            }

            int value = int.Parse(str);

            if(lastChar == 'D')
            {
                value *= -1;
            }

            return value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static long? UnpackDataInt64(ReadOnlySpan<byte> bytes)
        {
            string str = "";

            for(int i = 0; i < bytes.Length; ++i)
            {
                str += bytes[i].ToString("x2");
            }

            char lastChar = str[^1];

            if(lastChar.IsLetter())
            {
                str = str.Substring(0, str.Length - 1);
            }

            if(str.All(c => c == '9'))
            {
                return 0;
            }

            long value = long.Parse(str);

            if(lastChar == 'D')
            {
                value *= -1;
            }

            return value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static float? UnpackDataSingle(ReadOnlySpan<byte> bytes, int decimalPlace)
        {
            string str = "";

            for(int i = 0; i < bytes.Length; ++i)
            {
                str += bytes[i].ToString("x2");
            }

            char lastChar = str[^1];

            if(lastChar.IsLetter())
            {
                str = str.Substring(0, str.Length - 1);
            }

            if(str.All(c => c == '9'))
            {
                return 0.0f;
            }

            if(str.Length == 1 && str == "0")
            {
                str += ".0";
            }
            else
            {
                str = str.Insert(str.Length - decimalPlace, ".");
            }

            float value = float.Parse(str);

            if(lastChar == 'D')
            {
                value *= -1;
            }

            return value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static double? UnpackDataDouble(ReadOnlySpan<byte> bytes, int decimalPlace)
        {
            string str = "";

            for(int i = 0; i < bytes.Length; ++i)
            {
                str += bytes[i].ToString("x2");
            }

            char lastChar = str[^1];

            if(lastChar.IsLetter())
            {
                str = str.Substring(0, str.Length - 1);
            }

            if(str.All(c => c == '9'))
            {
                return 0.0;
            }

            if(str.Length == 1 && str == "0")
            {
                str += ".0";
            }
            else
            {
                str = str.Insert(str.Length - decimalPlace, ".");
            }

            double value = double.Parse(str);

            if(lastChar == 'D')
            {
                value *= -1;
            }

            return value;
        }
    }
}
