using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

using Kokkos;

namespace OilGas.Data
{
    public class CsvReader : IDisposable
    {
        private MemoryStream   _inputStream;
        private BufferedStream _inputBuffered;
        private StreamReader   _inputReader;

        public CsvReader(string data)
        {
            _inputStream   = new MemoryStream(Encoding.UTF8.GetBytes(data));
            _inputBuffered = new BufferedStream(_inputStream);
            _inputReader   = new StreamReader(_inputBuffered);
        }

        public CsvReader(byte[] bytes)
        {
            _inputStream   = new MemoryStream(bytes);
            _inputBuffered = new BufferedStream(_inputStream);
            _inputReader   = new StreamReader(_inputBuffered);
        }

        public void Dispose()
        {
            //GC.SuppressFinalize(true);

            if(_inputStream != null)
            {
                _inputStream.Close();
                _inputStream = null;
            }

            if(_inputBuffered != null)
            {
                _inputBuffered.Close();
                _inputBuffered = null;
            }

            if(_inputReader != null)
            {
                _inputReader.Close();
                _inputReader = null;
            }
        }

        ~CsvReader()
        {
            Dispose();
        }

        public (List<string[]> header, List<string[]> rows) ReadFile(int numberOfHeaderLines)
        {
            List<string[]> colDatas = new List<string[]>(numberOfHeaderLines);
            List<string[]> rowDatas;

            using(Measure.Execution())
            {
                using(_inputStream)
                {
                    using(_inputBuffered)
                    {
                        using(_inputReader)
                        {
                            ReadOnlySpan<char> data = _inputReader.ReadToEnd();

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
                    }
                }

            }

            return  (colDatas, rowDatas);
        }

        private static string[] Parser(ReadOnlySpan<char> values)
        {
            values =  TrimRight(values);

            MatchCollection matches = Regex.Matches(values.ToString() + ",",
                                                    "(\"[^\"]*(?:\"\"[^\"]*)*\"|[^,]*),", RegexOptions.Compiled);

            return matches.Select(Dequote).ToArray();
        }

        private static string TrimRight(ReadOnlySpan<char> src)
        {
            return Regex.Replace(src.ToString(),
                                 "(?:\x0D\x0A|[\x0D\x0A])?$",
                                 "",
                                 RegexOptions.Singleline | RegexOptions.Compiled);
        }

        private static string Dequote(Match match)
        {
            string s = match.Groups[1].Value;

            Match quoted = Regex.Match(s,
                                       "^\"(.*)\"$",
                                       RegexOptions.Singleline | RegexOptions.Compiled);

            if(quoted.Success)
            {
                return quoted.Groups[1].Value;//.Replace("\"\"","\"");
            }

            return s;
        }
    }
}