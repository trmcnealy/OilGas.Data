using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using Windows.Data.Pdf;
using Windows.Globalization;
using Windows.Graphics.Imaging;
using Windows.Media.Ocr;
using Windows.Storage;
using Windows.Storage.Streams;

namespace OilGas.Data.RRC.Texas
{
    public sealed class RrcTexasException : Exception
    {
        public RrcTexasException()
        {
        }

        public RrcTexasException(string message)
            : base(message)
        {
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void Throw()
        {
            throw new RrcTexasException();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void Throw(string message)
        {
            throw new RrcTexasException(message);
        }
    }

    public static class CompletionReportReader
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static double ReadDouble(List<OcrWord> words)
        {
            if(words.Count == 0)
            {
                return 0.0;
            }

            words = words.OrderBy(o => o.BoundingRect.Left).ToList();

            string input = words.Select(o => o.Text)
                                .Aggregate("",
                                           (current,
                                            next) => current + next)
                                .Replace("O", "0");

            if(double.TryParse(input, out double result))
            {
                return result;
            }

            foreach(OcrWord word in words)
            {
                if(double.TryParse(word.Text, out result))
                {
                    return result;
                }
            }

            RrcTexasException.Throw();

            return double.NaN;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static void ReadPdfs()
        {
            ////string file = @"D:\OilGasData\42-255-31650-00-00.G1.pdf";
            ////string pdfFile = @"D:\OilGasData\42-255-31655-00-00.G1.pdf";
            ////string file = @"D:\OilGasData\42-255-31594-00-00.G1.pdf";

            //string[] files = Directory.GetFiles(@"R:\OilGasData", "*.pdf");

            ////Dictionary<string, OilGasPropertiesQuery> oilGasProperties = new Dictionary<string, OilGasPropertiesQuery>();

            //List<(string, DepthQuery)> depthQueries = new List<(string, DepthQuery)>();

            //List<OcrFileData> ocrFilesData = new List<OcrFileData>();

            //ParallelOptions parallelOptions = new ParallelOptions
            //{
            //    MaxDegreeOfParallelism = Environment.ProcessorCount
            //};

            //Parallel.ForEach(Partitioner.Create(0, files.Length, files.Length / Environment.ProcessorCount),
            //                 parallelOptions,
            //                 row =>
            //                 {
            //                     string                 file;
            //                     OcrLine                line;
            //                     int                    j;
            //                     IReadOnlyList<OcrLine> lines;
            //                     Rect                   lineRectOffset;

            //                     for(int i = row.Item1; i < row.Item2; i++)
            //                     {
            //                         file = files[i];

            //                         lines          = ReadPdf(file);
            //                         lineRectOffset = Rect.Empty;

            //                         for(j = 0; j < lines.Count; ++j)
            //                         {
            //                             line = lines[j];

            //                             if(line.Text.Contains(DepthQuery.SectionLabel))
            //                             {
            //                                 foreach(OcrWord word in line.Words)
            //                                 {
            //                                     lineRectOffset.Union(word.BoundingRect);
            //                                 }

            //                                 break;
            //                             }
            //                         }

            //                         ocrFilesData.Add(new OcrFileData(file, j, lines, lineRectOffset));
            //                     }
            //                 });

            //Parallel.ForEach(ocrFilesData,
            //                 data =>
            //                 {
            //                     double? tvdDepth   = null;
            //                     double? totalDepth = null;

            //                     List<OcrWord> tvdDepthWords   = new List<OcrWord>();
            //                     List<OcrWord> totalDepthWords = new List<OcrWord>();

            //                     Parallel.ForEach(Partitioner.Create(data.StartIndex, data.Lines.Count),
            //                                      row =>
            //                                      {
            //                                          string _file = data.FileName;

            //                                          OcrLine line;
            //                                          //Rect    lineRect;

            //                                          double left, right, top, bottom;

            //                                          for(int i = row.Item1; i < row.Item2; i++)
            //                                          {
            //                                              line = data.Lines[i];

            //                                              foreach(OcrWord word in line.Words)
            //                                              {
            //                                                  left   = word.BoundingRect.Left;
            //                                                  top    = word.BoundingRect.Top - data.LineRectOffset.Top;
            //                                                  right  = word.BoundingRect.Right;
            //                                                  bottom = word.BoundingRect.Bottom - data.LineRectOffset.Top;

            //                                                  //lineRect.Union(word.BoundingRect);

            //                                                  //Console.WriteLine($"[{left},{top},{right},{bottom}]{word.Text}");

            //                                                  if(DepthQuery.TvdDepthBox.IsInBox(1, left, top, right, bottom))
            //                                                  {
            //                                                      tvdDepthWords.Add(word);
            //                                                  }
            //                                                  else if(DepthQuery.TotalDepthBox.IsInBox(1, left, top, right, bottom))
            //                                                  {
            //                                                      totalDepthWords.Add(word);
            //                                                  }
            //                                              }

            //                                              //Console.WriteLine($"[{lineRect.Left},{lineRect.Top},{lineRect.Right},{lineRect.Bottom}]{line.Text}");
            //                                          }
            //                                      });

            //                     if(tvdDepthWords.Count > 0)
            //                     {
            //                         tvdDepth = ReadValue(tvdDepthWords);
            //                     }

            //                     if(totalDepthWords.Count > 0)
            //                     {
            //                         totalDepth = ReadValue(totalDepthWords);
            //                     }

            //                     if(tvdDepth != null && totalDepth != null)
            //                     {
            //                         string api = Path.GetFileNameWithoutExtension(data.FileName).Substring(0, 18);

            //                         DepthQuery depthQuery = new DepthQuery(tvdDepth.Value, totalDepth.Value);

            //                         depthQueries.Add((api, depthQuery));
            //                     }
            //                 });

            //using(StreamWriter sw = new StreamWriter(@"D:\OilGasData\DepthQueries.txt"))
            //{
            //    foreach((string api, DepthQuery depthQuery) keyValue in depthQueries)
            //    {
            //        sw.WriteLine($"{keyValue.api},{keyValue.depthQuery}");
            //    }
            //}
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task SaveSoftwareBitmapToFile(SoftwareBitmap softwareBitmap,
                                                          StorageFile    outputFile)
        {
            using IRandomAccessStream stream = await outputFile.OpenAsync(FileAccessMode.ReadWrite);

            BitmapEncoder encoder = await BitmapEncoder.CreateAsync(BitmapEncoder.PngEncoderId, stream);

            encoder.SetSoftwareBitmap(softwareBitmap);

            //encoder.BitmapTransform.ScaledWidth  = (uint)softwareBitmap.PixelWidth;
            //encoder.BitmapTransform.ScaledHeight = (uint)softwareBitmap.PixelHeight;
            //encoder.BitmapTransform.Rotation          = Windows.Graphics.Imaging.BitmapRotation.Clockwise90Degrees;
            //encoder.BitmapTransform.InterpolationMode = BitmapInterpolationMode.Cubic;
            //encoder.IsThumbnailGenerated              = true;

            await encoder.FlushAsync();

            stream.Dispose();

            //try
            //{
            //    await encoder.FlushAsync();
            //}
            //catch(Exception err)
            //{
            //    //const int WINCODEC_ERR_UNSUPPORTEDOPERATION = unchecked((int)0x88982F81);

            //    //switch(err.HResult)
            //    //{
            //    //    case WINCODEC_ERR_UNSUPPORTEDOPERATION:
            //    //        // If the encoder does not support writing a thumbnail, then try again
            //    //        // but disable thumbnail generation.
            //    //        encoder.IsThumbnailGenerated = false;

            //    //        break;
            //    //    default: throw;
            //    //}
            //}

            //if(encoder.IsThumbnailGenerated == false)
            //{
            //    await encoder.FlushAsync();
            //}
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static List<IReadOnlyList<OcrLine>> ReadPdf(string pdfFile)
        {
            StorageFile file = StorageFile.GetFileFromPathAsync(pdfFile).GetAwaiter().GetResult();

            PdfDocument pdfDocument = PdfDocument.LoadFromFileAsync(file).GetAwaiter().GetResult();

            if(pdfDocument is null)
            {
                throw new NullReferenceException(nameof(pdfDocument));
            }

            List<IReadOnlyList<OcrLine>> ocrPages = new List<IReadOnlyList<OcrLine>>();

            Language                   ocrLanguage = new Language("en-US");
            OcrEngine                  ocrEngine   = OcrEngine.TryCreateFromUserProfileLanguages() ?? OcrEngine.TryCreateFromLanguage(ocrLanguage);
            InMemoryRandomAccessStream stream;
            BitmapDecoder              decoder;
            SoftwareBitmap             bitmap;
            OcrResult                  ocrResult;
            PdfPage                    page;

            for(uint pageNumber = 0; pageNumber < pdfDocument.PageCount; ++pageNumber)
            {
                page = pdfDocument.GetPage(pageNumber);

                stream = new InMemoryRandomAccessStream();

                page.RenderToStreamAsync(stream).GetAwaiter().GetResult();

                decoder = BitmapDecoder.CreateAsync(stream).GetAwaiter().GetResult();

                bitmap = decoder.GetSoftwareBitmapAsync().GetAwaiter().GetResult();

                //string newFile = Path.ChangeExtension(pdfFile, $".{pageNumber}.png");

                //if(!File.Exists(newFile))
                //{
                //    File.Create(newFile);
                //}

                //StorageFile imageFile = StorageFile.GetFileFromPathAsync(newFile).GetAwaiter().GetResult();

                //Windows.Storage.StorageFile sampleFile = await StorageFolder.GetFolderFromPathAsync() file.fol.CreateFileAsync("sample.txt", Windows.Storage.CreationCollisionOption.ReplaceExisting);

                //SaveSoftwareBitmapToFile(bitmap, imageFile).GetAwaiter().GetResult();

                //Console.WriteLine($"{pdfFile}:{bitmap.PixelWidth}x{bitmap.PixelHeight}");

                if(ocrEngine != null)
                {
                    try
                    {
                        ocrResult = ocrEngine.RecognizeAsync(bitmap).GetAwaiter().GetResult();

                        ocrPages.Add(ocrResult.Lines);
                    }
                    catch(Exception)
                    {
                        //
                    }
                }

                page.Dispose();
            }

            return ocrPages;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static (DepthScanner Depth, OilGasPropertiesScanner OilGasProperties) ReadReport(string file)
        {
            List<IReadOnlyList<OcrLine>> pages = ReadPdf(file);

            DepthScanner            depthScanner            = new DepthScanner();
            OilGasPropertiesScanner oilGasPropertiesScanner = new OilGasPropertiesScanner();

            depthScanner.FindAndSetSectionHeader(file, pages);

            Parallel.ForEach(Partitioner.Create(depthScanner.SectionData.StartIndex, depthScanner.SectionData.Lines.Count),
                             row =>
                             {
                                 OcrLine line;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     line = depthScanner.SectionData.Lines[i];

                                     foreach(OcrWord word in line.Words)
                                     {
                                         depthScanner.AddIfInBox(depthScanner.SectionData.PageNumber, word);
                                     }
                                 }
                             });

            depthScanner.BuildValues(ReadDouble);

            oilGasPropertiesScanner.FindAndSetSectionHeader(file, pages);

            Parallel.ForEach(Partitioner.Create(oilGasPropertiesScanner.SectionData.StartIndex, oilGasPropertiesScanner.SectionData.Lines.Count),
                             row =>
                             {
                                 OcrLine line;

                                 for(int i = row.Item1; i < row.Item2; i++)
                                 {
                                     line = oilGasPropertiesScanner.SectionData.Lines[i];

                                     foreach(OcrWord word in line.Words)
                                     {
                                         oilGasPropertiesScanner.AddIfInBox(oilGasPropertiesScanner.SectionData.PageNumber, word);
                                     }
                                 }
                             });

            oilGasPropertiesScanner.BuildValues(ReadDouble);

            return (depthScanner, oilGasPropertiesScanner);

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

            // using(StreamWriter sw = new StreamWriter(@"D:\OilGasData\DepthQueries.txt"))
            // {
            //     foreach((string api, DepthQuery depthQuery) keyValue in depthQueries)
            //     {
            //         sw.WriteLine($"{keyValue.api},{keyValue.depthQuery}");
            //     }
            // }
        }
    }
}
