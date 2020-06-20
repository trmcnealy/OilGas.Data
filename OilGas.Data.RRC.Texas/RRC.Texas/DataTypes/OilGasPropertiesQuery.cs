using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

using Windows.Foundation;
using Windows.Media.Ocr;

namespace OilGas.Data.RRC.Texas
{
    public interface IPdfHitBox
    {
        bool IsInBox(int    page,
                     double left,
                     double top,
                     double right,
                     double bottom);
    }

    public readonly struct PdfHitBox : IPdfHitBox
    {
        private readonly int _page;

        private readonly double _left;
        private readonly double _top;
        private readonly double _right;
        private readonly double _bottom;

        private readonly double _width;
        private readonly double _height;

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public PdfHitBox(int    page,
                         double left,
                         double top,
                         double right,
                         double bottom)
        {
            _page   = page;
            _left   = left;
            _top    = top;
            _right  = right;
            _bottom = bottom;

            _width  = _right  - _left;
            _height = _bottom - _top;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public bool IsInBox(int    page,
                            double left,
                            double top,
                            double right,
                            double bottom)
        {
            if(_page != page)
            {
                return false;
            }

            if(_left <= left && _top <= top && _right >= right && _bottom >= bottom)
            {
                return true;
            }

            return false;
        }
    }

    public interface IPdfHitVariable<T>
    {
        List<OcrWord> FoundWords { get; }

        PdfHitBox HitBox { get; set; }

        bool IsInBox(int    page,
                     double left,
                     double top,
                     double right,
                     double bottom);

        void SetValue(Func<List<OcrWord>, T> func);
    }

    public sealed unsafe class PdfHitVariable<T> : IPdfHitVariable<T>
        where T : unmanaged
    {
        public T* ValueRef { get; set; }

        public PdfHitVariable(PdfHitBox hitBox,
                              ref T     value)
        {
            HitBox = hitBox;

            ValueRef = (T*)Unsafe.AsPointer(ref value);
        }

        public List<OcrWord> FoundWords { get; } = new List<OcrWord>();

        public PdfHitBox HitBox { get; set; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public bool IsInBox(int    page,
                            double left,
                            double top,
                            double right,
                            double bottom)
        {
            return HitBox.IsInBox(page, left, top, right, bottom);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void SetValue(Func<List<OcrWord>, T> func)
        {
            *ValueRef = func(FoundWords);
        }
    }

    public interface IPdfHitArea
    {
        string SectionHeader { get; }

        OcrFileData SectionData { get; }

        //IEnumerable<PdfHitVariable<T>> HitVariables { get; }

        void FindAndSetSectionHeader(string                       file,
                                     List<IReadOnlyList<OcrLine>> pages);

        bool AddIfInBox(int     page,
                        OcrWord word);
    }

    public abstract class PdfHitArea : IPdfHitArea
    {
        public const int PdfPixelX = 1213;
        public const int PdfPixelY = 1814;

        public IEnumerable<PdfHitVariable<double>> HitVariables { get; set; }

        protected PdfHitArea(string sectionHeader)
        {
            SectionHeader = sectionHeader;
        }

        public string SectionHeader { get; set; }

        public OcrFileData SectionData { get; set; }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void FindAndSetSectionHeader(string                       file,
                                            List<IReadOnlyList<OcrLine>> pages)
        {
            OcrLine                line;
            int                    j;
            IReadOnlyList<OcrLine> lines;
            Rect                   lineRectOffset;

            for(int i = 0; i < pages.Count; ++i)
            {
                lines          = pages[i];
                lineRectOffset = Rect.Empty;

                for(j = 0; j < lines.Count; ++j)
                {
                    line = lines[j];

                    if(line.Text.Contains(SectionHeader))
                    {
                        foreach(OcrWord word in line.Words)
                        {
                            lineRectOffset.Union(word.BoundingRect);
                        }

                        SectionData = new OcrFileData(file, i + 1, j, lines, lineRectOffset);

                        break;
                    }
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public bool AddIfInBox(int     page,
                               OcrWord word)
        {
            double left   = word.BoundingRect.Left;
            double top    = word.BoundingRect.Top - SectionData.LineRectOffset.Top;
            double right  = word.BoundingRect.Right;
            double bottom = word.BoundingRect.Bottom - SectionData.LineRectOffset.Top;

            foreach(PdfHitVariable<double> hitVariable in HitVariables)
            {
                if(hitVariable.IsInBox(page, left, top, right, bottom))
                {
                    hitVariable.FoundWords.Add(word);

                    return true;
                }
            }

            return false;
        }

        protected void SetVariables(IEnumerable<PdfHitVariable<double>> hitVariables)
        {
            HitVariables = hitVariables;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void BuildValues(Func<List<OcrWord>, double> func)
        {
            foreach(PdfHitVariable<double> hitVariable in HitVariables)
            {
                hitVariable.SetValue(func);
            }
        }
    }

    public sealed class DepthScanner : PdfHitArea
    {
        public const string SectionLabel = "COMPLETION INFORMATION";

        public static readonly PdfHitBox TvdDepthBox   = new PdfHitBox(1, 240, 205, 560,  235);
        public static readonly PdfHitBox TotalDepthBox = new PdfHitBox(1, 800, 205, 1180, 235);

        public double TvdDepth;

        public double TotalDepth;

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public DepthScanner()
            : base(SectionLabel)
        {
            SetVariables(new[]
            {
                new PdfHitVariable<double>(TvdDepthBox, ref TvdDepth), new PdfHitVariable<double>(TotalDepthBox, ref TotalDepth)
            });
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void SetValues(double tvdDepth,
                              double totalDepth)
        {
            TvdDepth   = tvdDepth;
            TotalDepth = totalDepth;
        }

        public override string ToString()
        {
            return $"{TvdDepth},{TotalDepth}";
        }
    }

    public sealed class OilGasPropertiesScanner : PdfHitArea
    {
        public const string SectionLabel = "FIELD DATA AND PRESSURE CALCULATIONS";

        public static readonly PdfHitBox GasSpecificGravityBox    = new PdfHitBox(2, 210,  25, 580,  55);
        public static readonly PdfHitBox OilApiGravityBox         = new PdfHitBox(2, 1000, 25, 1180, 55);
        public static readonly PdfHitBox GasLiquidHydroRatioBox   = new PdfHitBox(2, 360,  55, 580,  85);
        public static readonly PdfHitBox AvgShutinTemperatureBox  = new PdfHitBox(2, 260,  85, 580,  115);
        public static readonly PdfHitBox BottomHoleTemperatureBox = new PdfHitBox(2, 890,  85, 1000, 115);
        public static readonly PdfHitBox BottomHoleDepthBox       = new PdfHitBox(2, 1060, 85, 1145, 115);

        public double GasSpecificGravity;

        public double OilApiGravity;

        public double GasLiquidHydroRatio;

        public double AvgShutinTemperature;

        public double BottomHoleTemperature;

        public double BottomHoleDepth;

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public OilGasPropertiesScanner()
            : base(SectionLabel)
        {
            SetVariables(new[]
            {
                new PdfHitVariable<double>(GasSpecificGravityBox,    ref GasSpecificGravity), new PdfHitVariable<double>(OilApiGravityBox,         ref OilApiGravity),
                new PdfHitVariable<double>(GasLiquidHydroRatioBox,   ref GasLiquidHydroRatio), new PdfHitVariable<double>(AvgShutinTemperatureBox, ref AvgShutinTemperature),
                new PdfHitVariable<double>(BottomHoleTemperatureBox, ref BottomHoleTemperature), new PdfHitVariable<double>(BottomHoleDepthBox,    ref BottomHoleDepth)
            });
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public void SetValues(double gasSpecificGravity,
                              double oilApiGravity,
                              double gasLiquidHydroRatio,
                              double avgShutinTemperature,
                              double bottomHoleTemperature,
                              double bottomHoleDepth)
        {
            GasSpecificGravity    = gasSpecificGravity;
            OilApiGravity         = oilApiGravity;
            GasLiquidHydroRatio   = gasLiquidHydroRatio;
            AvgShutinTemperature  = avgShutinTemperature;
            BottomHoleTemperature = bottomHoleTemperature;
            BottomHoleDepth       = bottomHoleDepth;
        }

        public override string ToString()
        {
            return $"{GasSpecificGravity},{OilApiGravity},{AvgShutinTemperature},{BottomHoleTemperature},{BottomHoleDepth}";
        }
    }

    public static class ProcessCompletionReports
    {
        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //private static IReadOnlyList<OcrLine> ReadPdf(string pdfFile)
        //{
        //    StorageFile file = StorageFile.GetFileFromPathAsync(pdfFile).GetAwaiter().GetResult();

        //    PdfDocument pdfDocument = PdfDocument.LoadFromFileAsync(file).GetAwaiter().GetResult();

        //    if(pdfDocument is null)
        //    {
        //        throw new NullReferenceException(nameof(pdfDocument));
        //    }

        //    uint pageCount = pdfDocument.PageCount;

        //    if(pageCount >= 1)
        //    {
        //        using(PdfPage page = pdfDocument.GetPage(1))
        //        {
        //            Language  ocrLanguage = new Language("en-US");
        //            OcrEngine ocrEngine   = OcrEngine.TryCreateFromUserProfileLanguages() ?? OcrEngine.TryCreateFromLanguage(ocrLanguage);

        //            InMemoryRandomAccessStream stream = new InMemoryRandomAccessStream();

        //            page.RenderToStreamAsync(stream).GetAwaiter().GetResult();

        //            BitmapDecoder decoder = BitmapDecoder.CreateAsync(stream).GetAwaiter().GetResult();

        //            SoftwareBitmap bitmap = decoder.GetSoftwareBitmapAsync().GetAwaiter().GetResult(); //.GetSoftwareBitmapAsync(BitmapPixelFormat.Rgba16, BitmapAlphaMode.Straight);

        //            Console.WriteLine($"{pdfFile}:{bitmap.PixelWidth}x{bitmap.PixelHeight}");

        //            OcrResult ocrResult = ocrEngine.RecognizeAsync(bitmap).GetAwaiter().GetResult();

        //            IReadOnlyList<OcrLine> lines = ocrResult.Lines;

        //            return lines;
        //        }
        //    }

        //    return null;
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //private static void Throw()
        //{
        //    throw new Exception();
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //private static double ReadValue(List<OcrWord> words)
        //{
        //    words = words.OrderBy(o => o.BoundingRect.Left).ToList();

        //    string input = words.Select(o => o.Text)
        //                        .Aggregate("",
        //                                   (current,
        //                                    next) => current + next)
        //                        .Replace("O", "0");

        //    if(double.TryParse(input, out double result))
        //    {
        //        return result;
        //    }

        //    foreach(OcrWord word in words)
        //    {
        //        if(double.TryParse(word.Text, out result))
        //        {
        //            return result;
        //        }
        //    }

        //    Throw();

        //    return double.NaN;
        //}

        //public readonly struct OcrFileData
        //{
        //    public readonly string                 FileName;
        //    public readonly int                    StartIndex;
        //    public readonly IReadOnlyList<OcrLine> Lines;
        //    public readonly Rect                   LineRectOffset;

        //    public OcrFileData(string                 fileName,
        //                       int                    startIndex,
        //                       IReadOnlyList<OcrLine> lines,
        //                       Rect                   lineRectOffset)
        //    {
        //        FileName       = fileName;
        //        StartIndex     = startIndex;
        //        Lines          = lines;
        //        LineRectOffset = lineRectOffset;
        //    }
        //}

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //private static void ReadPdfs()
        //{
        //    //string file = @"D:\OilGasData\42-255-31650-00-00.G1.pdf";
        //    //string pdfFile = @"D:\OilGasData\42-255-31655-00-00.G1.pdf";
        //    //string file = @"D:\OilGasData\42-255-35769-00-00.G1.pdf";

        //    string[] files = Directory.GetFiles(@"R:\OilGasData", "*.pdf");

        //    Dictionary<string, OilGasPropertiesQuery> oilGasProperties = new Dictionary<string, OilGasPropertiesQuery>();
        //List<(string, DepthQuery)> depthQueries = new List<(string, DepthQuery)>();

        //    List<OcrFileData> ocrFilesData = new List<OcrFileData>();

        //    ParallelOptions parallelOptions = new ParallelOptions
        //    {
        //        MaxDegreeOfParallelism = Environment.ProcessorCount
        //    };

        //    Parallel.ForEach(Partitioner.Create(0, files.Length, files.Length / Environment.ProcessorCount),
        //                     parallelOptions,
        //                     row =>
        //                     {
        //                         string file;

        //                         for(int i = row.Item1; i < row.Item2; i++)
        //                         {
        //                             file = files[i];

        //                             OcrLine                line;
        //                             int                    j;
        //                             IReadOnlyList<OcrLine> lines          = ReadPdf(file);
        //                             Rect                   lineRectOffset = Rect.Empty;

        //                             for(j = 0; j < lines.Count; ++j)
        //                             {
        //                                 line = lines[j];

        //                                 if(line.Text.Contains("FIELD DATA AND PRESSURE CALCULATIONS"))
        //                                 {
        //                                     foreach(OcrWord word in line.Words)
        //                                     {
        //                                         lineRectOffset.Union(word.BoundingRect);
        //                                     }

        //                                     break;
        //                                 }
        //                             }

        //                             ocrFilesData.Add(new OcrFileData(file, j, lines, lineRectOffset));
        //                         }
        //                     });

        //    Parallel.ForEach(ocrFilesData,
        //                     (OcrFileData data) =>
        //                     {
        //                         //IReadOnlyList<OcrLine> lines = ReadPdf(file).Result;

        //                         //OcrLine line;
        //                         //Rect    lineRectOffset = Rect.Empty;

        //                         //int i;

        //                         //for(i = 0; i < lines.Count; i++)
        //                         //{
        //                         //    line = lines[i];

        //                         //    if(line.Text.Contains("FIELD DATA AND PRESSURE CALCULATIONS"))
        //                         //    {
        //                         //        foreach(OcrWord word in line.Words)
        //                         //        {
        //                         //            lineRectOffset.Union(word.BoundingRect);
        //                         //        }

        //                         //        break;
        //                         //    }
        //                         //}

        //                         double? gasSpecificGravity    = null;
        //                         double? oilApiGravity         = null;
        //                         double? avgShutinTemperature  = null;
        //                         double? bottomHoleTemperature = null;
        //                         double? bottomHoleDepth       = null;

        //                         List<OcrWord> gasSpecificGravityWords    = new List<OcrWord>();
        //                         List<OcrWord> oilApiGravityWords         = new List<OcrWord>();
        //                         List<OcrWord> avgShutinTemperatureWords  = new List<OcrWord>();
        //                         List<OcrWord> bottomHoleTemperatureWords = new List<OcrWord>();
        //                         List<OcrWord> bottomHoleDepthWords       = new List<OcrWord>();

        // double? tvdDepth   = null;
        // double? totalDepth = null;
        //
        // List<OcrWord> tvdDepthWords   = new List<OcrWord>();
        // List<OcrWord> totalDepthWords = new List<OcrWord>();

        //                         Parallel.ForEach(Partitioner.Create(data.StartIndex, data.Lines.Count),
        //                                          row =>
        //                                          {
        //                                              string _file = data.FileName;

        //                                              OcrLine line;
        //                                              Rect    lineRect;

        //                                              double left, right, top, bottom;

        //                                              for(int i = row.Item1; i < row.Item2; i++)
        //                                              {
        //                                                  line     = data.Lines[i];
        //                                                  lineRect = Rect.Empty;

        //                                                  foreach(OcrWord word in line.Words)
        //                                                  {
        //                                                      left   = word.BoundingRect.Left;
        //                                                      top    = word.BoundingRect.Top - data.LineRectOffset.Top;
        //                                                      right  = word.BoundingRect.Right;
        //                                                      bottom = word.BoundingRect.Bottom - data.LineRectOffset.Top;

        //                                                      //lineRect.Union(word.BoundingRect);

        //                                                      //Console.WriteLine($"[{word.BoundingRect.Left},{word.BoundingRect.Top},{word.BoundingRect.Right},{word.BoundingRect.Bottom}]{word.Text}");
        //
        // if(DepthQuery.TvdDepthBox.IsInBox(1, left, top, right, bottom))
        // {
        //     tvdDepthWords.Add(word);
        // }
        // else if(DepthQuery.TotalDepthBox.IsInBox(1, left, top, right, bottom))
        // {
        //     totalDepthWords.Add(word);
        // }

        //                                                      if(OilGasPropertiesQuery.GasSpecificGravityBox.IsInBox(2, left, top, right, bottom))
        //                                                      {
        //                                                          gasSpecificGravityWords.Add(word);
        //                                                      }
        //                                                      else if(OilGasPropertiesQuery.OilApiGravityBox.IsInBox(2, left, top, right, bottom))
        //                                                      {
        //                                                          oilApiGravityWords.Add(word);
        //                                                      }
        //                                                      //else if(OilGasPropertiesQuery.GasLiquidHydroRatioBox.IsInBox(2,
        //                                                      //                                                             word.BoundingRect.Left,
        //                                                      //                                                             word.BoundingRect.Top,
        //                                                      //                                                             word.BoundingRect.Right,
        //                                                      //                                                             word.BoundingRect.Bottom))
        //                                                      //{
        //                                                      //}
        //                                                      else if(OilGasPropertiesQuery.AvgShutinTemperatureBox.IsInBox(2, left, top, right, bottom))
        //                                                      {
        //                                                          avgShutinTemperatureWords.Add(word);
        //                                                      }
        //                                                      else if(OilGasPropertiesQuery.BottomHoleTemperatureBox.IsInBox(2, left, top, right, bottom))
        //                                                      {
        //                                                          bottomHoleTemperatureWords.Add(word);
        //                                                      }
        //                                                      else if(OilGasPropertiesQuery.BottomHoleDepthBox.IsInBox(2, left, top, right, bottom))
        //                                                      {
        //                                                          bottomHoleDepthWords.Add(word);
        //                                                      }

        //                                                  }

        //                                                  //Console.WriteLine($"[{lineRect.Left},{lineRect.Top},{lineRect.Right},{lineRect.Bottom}]{line.Text}");
        //                                              }
        //                                          });
        // if(tvdDepthWords.Count > 0)
        // {
        //     tvdDepth = ReadValue(tvdDepthWords);
        // }
        //
        // if(totalDepthWords.Count > 0)
        // {
        //     totalDepth = ReadValue(totalDepthWords);
        // }
        //
        // if(tvdDepth != null && totalDepth != null)
        // {
        //     string api = Path.GetFileNameWithoutExtension(data.FileName).Substring(0, 18);
        //
        //     DepthQuery depthQuery = new DepthQuery(tvdDepth.Value, totalDepth.Value);
        //
        //     depthQueries.Add((api, depthQuery));
        // }
        //                         if(gasSpecificGravityWords.Count > 0)
        //                         {
        //                             gasSpecificGravity = ReadValue(gasSpecificGravityWords);
        //                         }

        //                         if(oilApiGravityWords.Count > 0)
        //                         {
        //                             oilApiGravity = ReadValue(oilApiGravityWords);
        //                         }

        //                         if(avgShutinTemperatureWords.Count > 0)
        //                         {
        //                             avgShutinTemperature = ReadValue(avgShutinTemperatureWords);
        //                         }

        //                         if(bottomHoleTemperatureWords.Count > 0)
        //                         {
        //                             bottomHoleTemperature = ReadValue(bottomHoleTemperatureWords);
        //                         }

        //                         if(bottomHoleDepthWords.Count > 0)
        //                         {
        //                             bottomHoleDepth = ReadValue(bottomHoleDepthWords);
        //                         }

        //                         if(gasSpecificGravity != null)
        //                         {
        //                             string api = Path.GetFileNameWithoutExtension(data.FileName).Substring(0, 18);

        //                             OilGasPropertiesQuery oilGasPropertiesQuery = new OilGasPropertiesQuery(gasSpecificGravity.Value,
        //                                                                                                     oilApiGravity         ?? 0.0,
        //                                                                                                     avgShutinTemperature  ?? 0.0,
        //                                                                                                     bottomHoleTemperature ?? 0.0,
        //                                                                                                     bottomHoleDepth       ?? 0.0);

        //                             oilGasProperties.Add(api, oilGasPropertiesQuery);
        //                         }
        //                     });

        // using(StreamWriter sw = new StreamWriter(@"D:\OilGasData\DepthQueries.txt"))
        // {
        //     foreach((string api, DepthQuery depthQuery) keyValue in depthQueries)
        //     {
        //         sw.WriteLine($"{keyValue.api},{keyValue.depthQuery}");
        //     }
        // }

        //    using(StreamWriter sw = new StreamWriter(@"D:\OilGasData\OilGasProperties.txt"))
        //    {
        //        foreach(KeyValuePair<string, OilGasPropertiesQuery> keyValue in oilGasProperties)
        //        {
        //            sw.WriteLine($"{keyValue.Key},{keyValue.Value}");
        //        }
        //    }

        //}
    }
}
