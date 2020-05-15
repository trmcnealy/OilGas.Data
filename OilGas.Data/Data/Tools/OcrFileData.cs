using System.Collections.Generic;

using Windows.Data.Pdf;
using Windows.Foundation;
using Windows.Globalization;
using Windows.Graphics.Imaging;
using Windows.Media.Ocr;
using Windows.Storage;
using Windows.Storage.Streams;

namespace OilGas.Data
{
    public readonly struct OcrFileData
    {
        public readonly string                 FileName;
        public readonly int                    PageNumber;
        public readonly int                    StartIndex;
        public readonly IReadOnlyList<OcrLine> Lines;
        public readonly Rect                   LineRectOffset;

        public OcrFileData(string                 fileName,
                           int                    pageNumber,
                           int                    startIndex,
                           IReadOnlyList<OcrLine> lines,
                           Rect                   lineRectOffset)
        {
            FileName       = fileName;
            PageNumber     = pageNumber;
            StartIndex     = startIndex;
            Lines          = lines;
            LineRectOffset = lineRectOffset;
        }
    }
}
