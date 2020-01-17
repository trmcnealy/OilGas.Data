using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace OilGas.Data
{
    public class CsvReader : IDisposable
    {
        private MemoryStream   _inputStream;
        private BufferedStream _inputBuffered;
        private StreamReader   _inputReader;

        public CsvReader(string data)
        {
            _inputStream = new MemoryStream(Encoding.UTF8.GetBytes(data));

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

        public List<string[]> ReadFile(int numberOfHeaderLines)
        {
            List<string[]> rowDatas;

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

                        rowDatas = new List<string[]>(lineEndings.Count);

                        int start;
                        int end;

                        for(int i = 0; i < lineEndings.Count - 1; ++i)
                        {
                            start = lineEndings[i];
                            end   = lineEndings[i + 1];

                            string[] valueArray = Parser(data.Slice(start,
                                                                    end - start).ToString());

                            rowDatas.Add(valueArray);
                        }
                    }
                }
            }

            return rowDatas;
        }

        private static string[] Parser(string values)
        {
            values =  TrimRight(values);
            values += ",";

            MatchCollection matches = Regex.Matches(values,
                                                    "(\"[^\"]*(?:\"\"[^\"]*)*\"|[^,]*),");

            return matches.Select(Dequote).ToArray();
        }

        private static string TrimRight(string src)
        {
            return Regex.Replace(src,
                                 "(?:\x0D\x0A|[\x0D\x0A])?$",
                                 "",
                                 RegexOptions.Singleline);
        }

        private static string Dequote(Match match)
        {
            string s = match.Groups[1].Value;

            Match quoted = Regex.Match(s,
                                       "^\"(.*)\"$",
                                       RegexOptions.Singleline);

            if(quoted.Success)
            {
                return quoted.Groups[1].Value.Replace("\"\"",
                                                      "\"");
            }

            return s;
        }
    }
}