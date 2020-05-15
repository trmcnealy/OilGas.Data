#nullable enable

using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace OilGas.Data
{
    [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Ansi, Pack = 1)]
    public readonly struct SbnHeader
    {
        public const int MajicFileCode = 9994;
        public const int MajicUnknown  = -400;

        [FieldOffset(0)]
        private readonly int FileCode;

        [FieldOffset(4)]
        private readonly int Unknown0;

        [FieldOffset(8)]
        private readonly int Unused0;

        [FieldOffset(12)]
        private readonly int Unused1;

        [FieldOffset(16)]
        private readonly int Unused2;

        [FieldOffset(20)]
        private readonly int Unused3;

        [FieldOffset(24)]
        private readonly int FileLength;

        [FieldOffset(28)]
        private readonly int NumRecords;

        [FieldOffset(32)]
        private readonly double xmin;

        [FieldOffset(40)]
        private readonly double ymin;

        [FieldOffset(48)]
        private readonly double xmax;

        [FieldOffset(56)]
        private readonly double ymax;

        [FieldOffset(64)]
        private readonly double zmin;

        [FieldOffset(72)]
        private readonly double zmax;

        [FieldOffset(80)]
        private readonly double mmin;

        [FieldOffset(88)]
        private readonly double mmax;

        [FieldOffset(96)]
        private readonly int Unknown1;
    }

    [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Ansi, Pack = 1)]
    public readonly struct SbnRecordsHeader
    {
        [FieldOffset(0)]
        private readonly int RecNumber;

        [FieldOffset(4)]
        private readonly int RecordLength;

        [FieldOffset(8)]
        private readonly int BinNumber;

        [FieldOffset(12)]
        private readonly int ShapeCount;
    }

    [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Ansi, Pack = 1)]
    public readonly struct SbnRecords
    {
        [FieldOffset(0)]
        private readonly int RecNumber;

        [FieldOffset(4)]
        private readonly int RecordLength;

        [FieldOffset(8)]
        private readonly byte XMin;

        [FieldOffset(9)]
        private readonly byte YMin;

        [FieldOffset(10)]
        private readonly byte XMax;

        [FieldOffset(11)]
        private readonly byte YMax;

        [FieldOffset(12)]
        private readonly int ShpRecNum;
    }





    public class Dbase
    {
        public static DataTable Load(string dbfFilePath)
        {
            long         start = DateTime.Now.Ticks;
            DataTable    dt    = new DataTable();
            BinaryReader recReader;
            string       number;
            string       year;
            string       month;
            string       day;
            long         lDate;
            long         lTime;
            DataRow      row;
            int          fieldIndex;

            // If there isn't even a file, just return an empty DataTable
            if(false == File.Exists(dbfFilePath))
            {
                return dt;
            }

            BinaryReader? br = null;

            try
            {
                // Read the header into a buffer
                br = new BinaryReader(File.OpenRead(dbfFilePath));
                byte[] buffer = br.ReadBytes(Marshal.SizeOf(typeof(DBFHeader)));

                DBFHeader header = Serializer.Deserialize<DBFHeader>(buffer);

                //// Marshall the header into a DBFHeader structure
                //GCHandle  handle = GCHandle.Alloc(buffer, GCHandleType.Pinned);
                //DBFHeader header = (DBFHeader)Marshal.PtrToStructure(handle.AddrOfPinnedObject(), typeof(DBFHeader));
                //handle.Free();

                // Read in all the field descriptors. Per the spec, 13 (0D) marks the end of the field descriptors
                List<FieldDescriptor> fields = new List<FieldDescriptor>();

                while(13 != br.PeekChar())
                {
                    buffer = br.ReadBytes(Marshal.SizeOf(typeof(FieldDescriptor)));
                    //handle = GCHandle.Alloc(buffer, GCHandleType.Pinned);
                    //fields.Add((FieldDescriptor)Marshal.PtrToStructure(handle.AddrOfPinnedObject(), typeof(FieldDescriptor)));
                    //handle.Free();

                    FieldDescriptor fd = Serializer.Deserialize<FieldDescriptor>(buffer);

                    fields.Add(fd);
                }

                // Read in the first row of records, we need this to help determine column types below
                ((FileStream)br.BaseStream).Seek(header.headerLen + 1, SeekOrigin.Begin);
                buffer    = br.ReadBytes(header.recordLen);
                recReader = new BinaryReader(new MemoryStream(buffer));

                // Create the columns in our new DataTable
                DataColumn? col = null;

                foreach(FieldDescriptor field in fields)
                {
                    number = Encoding.ASCII.GetString(recReader.ReadBytes(field.fieldLen));

                    switch(field.fieldType)
                    {
                        case (sbyte)'N':
                            if(number.IndexOf(".", StringComparison.Ordinal) > -1)
                            {
                                col = new DataColumn(field.FieldName, typeof(double));
                            }
                            else
                            {
                                col = new DataColumn(field.FieldName, typeof(long));
                            }

                            break;
                        case (sbyte)'C':
                            col = new DataColumn(field.FieldName, typeof(string));

                            break;
                        case (sbyte)'T':
                            // You can uncomment this to see the time component in the grid
                            //col = new DataColumn(field.FieldName, typeof(string));
                            col = new DataColumn(field.FieldName, typeof(DateTime));

                            break;
                        case (sbyte)'D':
                            col = new DataColumn(field.FieldName, typeof(DateTime));

                            break;
                        case (sbyte)'L':
                            col = new DataColumn(field.FieldName, typeof(bool));

                            break;
                        case (sbyte)'F':
                            col = new DataColumn(field.FieldName, typeof(double));

                            break;
                    }

                    dt.Columns.Add(col);
                }

                // Skip past the end of the header. 
                ((FileStream)br.BaseStream).Seek(header.headerLen, SeekOrigin.Begin);

                // Read in all the records
                for(int counter = 0; counter <= header.numRecords - 1; counter++)
                {
                    // First we'll read the entire record into a buffer and then read each field from the buffer
                    // This helps account for any extra space at the end of each record and probably performs better
                    buffer    = br.ReadBytes(header.recordLen);
                    recReader = new BinaryReader(new MemoryStream(buffer));

                    // All dbf field records begin with a deleted flag field. Deleted - 0x2A (asterisk) else 0x20 (space)
                    if(recReader.ReadChar() == '*')
                    {
                        continue;
                    }

                    // Loop through each field in a record
                    fieldIndex = 0;
                    row        = dt.NewRow();

                    foreach(FieldDescriptor field in fields)
                    {
                        switch(field.fieldType)
                        {
                            case (sbyte)'N': // Number
                                // If you port this to .NET 2.0, use the Decimal.TryParse method
                                number = Encoding.ASCII.GetString(recReader.ReadBytes(field.fieldLen));

                                if(IsNumber(number))
                                {
                                    if(number.IndexOf(".", StringComparison.Ordinal) > -1)
                                    {
                                        row[fieldIndex] = double.Parse(number);
                                    }
                                    else
                                    {
                                        row[fieldIndex] = long.Parse(number);
                                    }
                                }
                                else
                                {
                                    throw new Exception();
                                }

                                break;

                            case (sbyte)'C': // String
                                row[fieldIndex] = Encoding.ASCII.GetString(recReader.ReadBytes(field.fieldLen));

                                break;

                            case (sbyte)'D': // Date (YYYYMMDD)
                                year            = Encoding.ASCII.GetString(recReader.ReadBytes(4));
                                month           = Encoding.ASCII.GetString(recReader.ReadBytes(2));
                                day             = Encoding.ASCII.GetString(recReader.ReadBytes(2));
                                row[fieldIndex] = DBNull.Value;

                                try
                                {
                                    if(IsNumber(year) && IsNumber(month) && IsNumber(day))
                                    {
                                        if(int.Parse(year) > 1900)
                                        {
                                            row[fieldIndex] = new DateTime(int.Parse(year), int.Parse(month), int.Parse(day));
                                        }
                                    }
                                }
                                catch
                                {
                                    // ignored
                                }

                                break;

                            case (sbyte)'T': // Timestamp, 8 bytes - two integers, first for date, second for time
                                // Date is the number of days since 01/01/4713 BC (Julian Days)
                                // Time is hours * 3600000L + minutes * 60000L + Seconds * 1000L (Milliseconds since midnight)
                                lDate           = recReader.ReadInt32();
                                lTime           = recReader.ReadInt32() * 10000L;
                                row[fieldIndex] = JulianToDateTime(lDate).AddTicks(lTime);

                                break;

                            case (sbyte)'L': // Boolean (Y/N)
                                if('Y' == recReader.ReadByte())
                                {
                                    row[fieldIndex] = true;
                                }
                                else
                                {
                                    row[fieldIndex] = false;
                                }

                                break;

                            case (sbyte)'F':
                                number = Encoding.ASCII.GetString(recReader.ReadBytes(field.fieldLen));

                                if(IsNumber(number))
                                {
                                    row[fieldIndex] = double.Parse(number);
                                }
                                else
                                {
                                    throw new Exception();
                                }

                                break;
                        }

                        fieldIndex++;
                    }

                    recReader.Close();
                    dt.Rows.Add(row);
                }
            }

            finally
            {
                br?.Close();
            }

            long count = DateTime.Now.Ticks - start;

            return dt;
        }

        /// <summary>
        ///     Simple function to test is a string can be parsed. There may be a better way, but this works If you port this
        ///     to .NET 2.0, use the new TryParse methods instead of this *Thanks to wu.qingman on code project for fixing a bug in
        ///     this for me
        /// </summary>
        /// <returns>true if string can be parsed</returns>
        public static bool IsNumber(string numberString)
        {
            char[] numbers      = numberString.ToCharArray();
            int    number_count = 0;
            int    point_count  = 0;
            int    space_count  = 0;

            foreach(char number in numbers)
            {
                if(number >= 48 && number <= 57)
                {
                    number_count += 1;
                }
                else if(number == 46)
                {
                    point_count += 1;
                }
                else if(number == 45)
                {
                    // minus sign
                }
                else if(number == 32)
                {
                    space_count += 1;
                }
                else
                {
                    return false;
                }
            }

            return number_count > 0 && point_count < 2;
        }

        /// <summary>
        ///     Convert a Julian Date to a .NET DateTime structure Implemented from pseudo code at
        ///     http://en.wikipedia.org/wiki/Julian_day
        /// </summary>
        /// <param name="lJDN">Julian Date to convert (days since 01/01/4713 BC)</param>
        /// <returns>DateTime</returns>
        private static DateTime JulianToDateTime(long lJDN)
        {
            double p  = Convert.ToDouble(lJDN);
            double s1 = p + 68569;
            double n  = Math.Floor(4                     * s1 / 146097);
            double s2 = s1 - Math.Floor((146097 * n + 3) / 4);
            double i  = Math.Floor(4000                  * (s2 + 1) / 1461001);
            double s3 = s2 - Math.Floor(1461 * i / 4) + 31;
            double q  = Math.Floor(80            * s3 / 2447);
            double d  = s3 - Math.Floor(2447 * q / 80);
            double s4 = Math.Floor(q / 11);
            double m  = q + 2              - 12 * s4;
            double j  = 100 * (n - 49) + i + s4;

            return new DateTime(Convert.ToInt32(j), Convert.ToInt32(m), Convert.ToInt32(d));
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi, Pack = 1)]
        public readonly struct DBFHeader
        {
            public readonly byte  version;
            public readonly byte  updateYear;
            public readonly byte  updateMonth;
            public readonly byte  updateDay;
            public readonly int   numRecords;
            public readonly short headerLen;
            public readonly short recordLen;

            public readonly short reserved1;
            public readonly byte  incompleteTrans;
            public readonly byte  encryptionFlag;
            public readonly int   reserved2;
            public readonly long  reserved3;
            public readonly byte  MDX;
            public readonly byte  language;
            public readonly short reserved4;
        }

        [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Ansi, Pack = 1, Size = 11 * sizeof(sbyte))]
        public readonly struct FixedString11
        {
            [FieldOffset(0)]
            public readonly sbyte _0;

            [FieldOffset(sizeof(sbyte))]
            public readonly sbyte _1;

            [FieldOffset(sizeof(sbyte) * 2)]
            public readonly sbyte _2;

            [FieldOffset(sizeof(sbyte) * 3)]
            public readonly sbyte _3;

            [FieldOffset(sizeof(sbyte) * 4)]
            public readonly sbyte _4;

            [FieldOffset(sizeof(sbyte) * 5)]
            public readonly sbyte _5;

            [FieldOffset(sizeof(sbyte) * 6)]
            public readonly sbyte _6;

            [FieldOffset(sizeof(sbyte) * 7)]
            public readonly sbyte _7;

            [FieldOffset(sizeof(sbyte) * 8)]
            public readonly sbyte _8;

            [FieldOffset(sizeof(sbyte) * 9)]
            public readonly sbyte _9;

            [FieldOffset(sizeof(sbyte) * 10)]
            public readonly sbyte _10;
        }

        
        [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Ansi, Pack = 1, Size = 7 * sizeof(byte))]
        public readonly struct FixedByteArray7
        {
            [FieldOffset(0)]
            public readonly sbyte _0;

            [FieldOffset(sizeof(sbyte))]
            public readonly sbyte _1;

            [FieldOffset(sizeof(sbyte) * 2)]
            public readonly sbyte _2;

            [FieldOffset(sizeof(sbyte) * 3)]
            public readonly sbyte _3;

            [FieldOffset(sizeof(sbyte) * 4)]
            public readonly sbyte _4;

            [FieldOffset(sizeof(sbyte) * 5)]
            public readonly sbyte _5;

            [FieldOffset(sizeof(sbyte) * 6)]
            public readonly sbyte _6;
        }
        
        /// <summary>https://www.dbase.com/Knowledgebase/INT/db7_file_fmt.htm</summary>
        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi, Pack = 1)]
        public readonly  struct FieldDescriptor
        {
            public readonly FixedString11 fieldName;
            public readonly sbyte fieldType;
            public readonly int   address;
            public readonly byte  fieldLen;
            public readonly byte  count;
            public readonly short reserved1;
            public readonly byte  workArea;
            public readonly short reserved2;
            public readonly byte  flag;
            public readonly FixedByteArray7 reserved3;
            public readonly byte indexFlag;

            public string FieldName
            {
                get
                {
                    unsafe
                    {
                        fixed(sbyte* fieldNamePtr = &fieldName._0)
                        {
                            return new string(fieldNamePtr);
                        }
                    }
                }
            }
        }

        //public static void Load(string filePath)
        //{
        //    string constr = $"Provider=Microsoft.Jet.OLEDB.4.0;Data Source={filePath};Extended Properties=dBASE IV;User ID=Admin;Password=;";
        //
        //    using(OleDbConnection con = new OleDbConnection(constr))
        //    {
        //        string       sql = "select * from " + fileName;
        //        OleDbCommand cmd = new OleDbCommand(sql, con);
        //        con.Open();
        //        DataSet          ds = new DataSet();
        //        OleDbDataAdapter da = new OleDbDataAdapter(cmd);
        //        da.Fill(ds);
        //    }
        //}

        public static class Serializer
        {
            public static unsafe byte[] Serialize<T>(T value)
                where T : unmanaged
            {
                byte[] buffer = new byte[sizeof(T)];

                fixed(byte* bufferPtr = buffer)
                {
                    Buffer.MemoryCopy(&value, bufferPtr, sizeof(T), sizeof(T));
                }

                return buffer;
            }

            public static unsafe T Deserialize<T>(byte[] buffer)
                where T : unmanaged
            {
                T result = new T();

                fixed(byte* bufferPtr = buffer)
                {
                    Buffer.MemoryCopy(bufferPtr, &result, sizeof(T), sizeof(T));
                }

                return result;
            }
        }
    }
}
