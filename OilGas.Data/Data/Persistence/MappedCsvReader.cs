using System;
using System.Buffers.Text;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;

using Kokkos;

namespace OilGas.Data
{
    public class MappedCsvReader
    {
        private readonly MemoryMapped _mm;

        public MappedCsvReader(MemoryMapped mm)
        {
            _mm = mm;
        }

        public (List<string[]> header, List<string[]> rows) ReadFile(int numberOfHeaderLines)
        {
            List<string[]> colDatas = new List<string[]>(numberOfHeaderLines);
            List<string[]> rowDatas;

            ReadOnlySpan<byte> data = _mm.GetDataPointer();

            using(Measure.Execution())
            {
                int length = data.Length;

                List<int> lineEndings = new List<int>(length / 30);

                int headerLines = 0;

                lineEndings.Add(0);

                for(int i = 0; i < length; ++i)
                {
                    if(headerLines < numberOfHeaderLines)
                    {
                        if(data[i] == '\n')
                        {
                            lineEndings[0] = i + 1;
                            ++headerLines;
                        }

                        continue;
                    }

                    if(data[i] == '\r' && data[i + 1] == '\n')
                    {
                        lineEndings.Add(++i + 1);
                        ++i;
                    }
                    else if(data[i - 1] != '\r' && data[i] == '\n')
                    {
                        lineEndings.Add(++i);
                    }
                }

                string[] valueArray;

                for(int i = 0; i < 1; ++i)
                {
                    valueArray = Parser(data.Slice(0, lineEndings[0]));

                    colDatas.Add(valueArray);
                }

                rowDatas = new List<string[]>(lineEndings.Count);

                int start;
                int end;

                for(int i = 0; i < lineEndings.Count - 1; ++i)
                {
                    start = lineEndings[i];
                    end   = lineEndings[i + 1];

                    valueArray = Parser(data.Slice(start, end - start));

                    rowDatas.Add(valueArray);
                }
            }

            return (colDatas, rowDatas);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static string[] Parser(ReadOnlySpan<byte> buffer)
        {
            string values = Encoding.Default.GetString(buffer);

            values = TrimRight(values);

            MatchCollection matches = Regex.Matches(values + ",", "(\"[^\"]*(?:\"\"[^\"]*)*\"|[^,]*),", RegexOptions.Compiled);

            return matches.Select(Dequote).ToArray();
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static string TrimRight(string values)
        {
            return Regex.Replace(values, "(?:\x0D\x0A|[\x0D\x0A])?$", "", RegexOptions.Singleline | RegexOptions.Compiled);
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static string Dequote(Match match)
        {
            string s = match.Groups[1].Value;

            Match quoted = Regex.Match(s, "^\"(.*)\"$", RegexOptions.Singleline | RegexOptions.Compiled);

            if(quoted.Success)
            {
                return quoted.Groups[1].Value; //.Replace("\"\"","\"");
            }

            return s;
        }
    }
}
