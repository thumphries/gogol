{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Slides.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Slides.Types
    (
    -- * Service Configuration
      slidesService

    -- * OAuth Scopes
    , spreadsheetsReadOnlyScope
    , presentationsScope
    , driveReadOnlyScope
    , driveScope
    , driveFileScope
    , spreadsheetsScope
    , presentationsReadOnlyScope

    -- * ParagraphMarker
    , ParagraphMarker
    , paragraphMarker
    , pmStyle
    , pmBullet

    -- * DeleteTableRowRequest
    , DeleteTableRowRequest
    , deleteTableRowRequest
    , dtrrCellLocation
    , dtrrTableObjectId

    -- * Thumbnail
    , Thumbnail
    , thumbnail
    , tHeight
    , tWidth
    , tContentURL

    -- * TableBOrderCell
    , TableBOrderCell
    , tableBOrderCell
    , tbocLocation
    , tbocTableBOrderProperties

    -- * ParagraphStyleDirection
    , ParagraphStyleDirection (..)

    -- * PageElementProperties
    , PageElementProperties
    , pageElementProperties
    , pepTransform
    , pepSize
    , pepPageObjectId

    -- * TextStyleBaselineOffSet
    , TextStyleBaselineOffSet (..)

    -- * ReplaceAllShapesWithImageResponse
    , ReplaceAllShapesWithImageResponse
    , replaceAllShapesWithImageResponse
    , raswirOccurrencesChanged

    -- * TableCellPropertiesContentAlignment
    , TableCellPropertiesContentAlignment (..)

    -- * RangeType
    , RangeType (..)

    -- * OutlineFill
    , OutlineFill
    , outlineFill
    , ofSolidFill

    -- * Image
    , Image
    , image
    , iImageProperties
    , iContentURL
    , iSourceURL

    -- * UpdateLinePropertiesRequest
    , UpdateLinePropertiesRequest
    , updateLinePropertiesRequest
    , ulprLineProperties
    , ulprObjectId
    , ulprFields

    -- * CropProperties
    , CropProperties
    , cropProperties
    , cpBottomOffSet
    , cpTopOffSet
    , cpAngle
    , cpRightOffSet
    , cpLeftOffSet

    -- * LineProperties
    , LineProperties
    , lineProperties
    , lpWeight
    , lpLink
    , lpDashStyle
    , lpStartArrow
    , lpLineFill
    , lpEndArrow

    -- * Group
    , Group
    , group'
    , gChildren

    -- * ReplaceImageRequest
    , ReplaceImageRequest
    , replaceImageRequest
    , rirImageReplaceMethod
    , rirImageObjectId
    , rirURL

    -- * BatchUpdatePresentationRequest
    , BatchUpdatePresentationRequest
    , batchUpdatePresentationRequest
    , buprRequests
    , buprWriteControl

    -- * CreateShapeRequest
    , CreateShapeRequest
    , createShapeRequest
    , csrShapeType
    , csrObjectId
    , csrElementProperties

    -- * PageBackgRoundFillPropertyState
    , PageBackgRoundFillPropertyState (..)

    -- * AutoText
    , AutoText
    , autoText
    , atStyle
    , atContent
    , atType

    -- * ReplaceAllShapesWithSheetsChartRequest
    , ReplaceAllShapesWithSheetsChartRequest
    , replaceAllShapesWithSheetsChartRequest
    , raswscrPageObjectIds
    , raswscrSpreadsheetId
    , raswscrLinkingMode
    , raswscrContainsText
    , raswscrChartId

    -- * List
    , List
    , list
    , lListId
    , lNestingLevel

    -- * NotesProperties
    , NotesProperties
    , notesProperties
    , npSpeakerNotesObjectId

    -- * GroupObjectsResponse
    , GroupObjectsResponse
    , groupObjectsResponse
    , gorObjectId

    -- * RgbColor
    , RgbColor
    , rgbColor
    , rcRed
    , rcGreen
    , rcBlue

    -- * UpdatePagePropertiesRequest
    , UpdatePagePropertiesRequest
    , updatePagePropertiesRequest
    , upprObjectId
    , upprPageProperties
    , upprFields

    -- * CreateSheetsChartRequest
    , CreateSheetsChartRequest
    , createSheetsChartRequest
    , cscrObjectId
    , cscrSpreadsheetId
    , cscrLinkingMode
    , cscrElementProperties
    , cscrChartId

    -- * TableRowProperties
    , TableRowProperties
    , tableRowProperties
    , trpMinRowHeight

    -- * UpdateTableRowPropertiesRequest
    , UpdateTableRowPropertiesRequest
    , updateTableRowPropertiesRequest
    , utrprTableRowProperties
    , utrprRowIndices
    , utrprObjectId
    , utrprFields

    -- * RecolorName
    , RecolorName (..)

    -- * CreateParagraphBulletsRequestBulletPreset
    , CreateParagraphBulletsRequestBulletPreset (..)

    -- * CreateShapeRequestShapeType
    , CreateShapeRequestShapeType (..)

    -- * LayoutReferencePredefinedLayout
    , LayoutReferencePredefinedLayout (..)

    -- * MasterProperties
    , MasterProperties
    , masterProperties
    , mpDisplayName

    -- * DeleteTextRequest
    , DeleteTextRequest
    , deleteTextRequest
    , dtrTextRange
    , dtrObjectId
    , dtrCellLocation

    -- * InsertTableColumnsRequest
    , InsertTableColumnsRequest
    , insertTableColumnsRequest
    , itcrInsertRight
    , itcrNumber
    , itcrCellLocation
    , itcrTableObjectId

    -- * TextContentLists
    , TextContentLists
    , textContentLists
    , tclAddtional

    -- * Size
    , Size
    , size
    , sHeight
    , sWidth

    -- * StretchedPictureFill
    , StretchedPictureFill
    , stretchedPictureFill
    , spfSize
    , spfContentURL

    -- * CreateLineRequestLineCategory
    , CreateLineRequestLineCategory (..)

    -- * TableBOrderFill
    , TableBOrderFill
    , tableBOrderFill
    , tbofSolidFill

    -- * SheetsChart
    , SheetsChart
    , sheetsChart
    , scSpreadsheetId
    , scContentURL
    , scSheetsChartProperties
    , scChartId

    -- * CreateShapeResponse
    , CreateShapeResponse
    , createShapeResponse
    , cObjectId

    -- * DeleteTableColumnRequest
    , DeleteTableColumnRequest
    , deleteTableColumnRequest
    , dtcrCellLocation
    , dtcrTableObjectId

    -- * TableBOrderRow
    , TableBOrderRow
    , tableBOrderRow
    , tborTableBOrderCells

    -- * ShapeShapeType
    , ShapeShapeType (..)

    -- * ShapePropertiesContentAlignment
    , ShapePropertiesContentAlignment (..)

    -- * OutlinePropertyState
    , OutlinePropertyState (..)

    -- * Link
    , Link
    , link
    , lURL
    , lPageObjectId
    , lRelativeLink
    , lSlideIndex

    -- * GroupObjectsRequest
    , GroupObjectsRequest
    , groupObjectsRequest
    , gorGroupObjectId
    , gorChildrenObjectIds

    -- * Dimension
    , Dimension
    , dimension
    , dMagnitude
    , dUnit

    -- * BatchUpdatePresentationResponse
    , BatchUpdatePresentationResponse
    , batchUpdatePresentationResponse
    , bPresentationId
    , bReplies
    , bWriteControl

    -- * DuplicateObjectRequestObjectIds
    , DuplicateObjectRequestObjectIds
    , duplicateObjectRequestObjectIds
    , doroiAddtional

    -- * ReplaceAllShapesWithSheetsChartResponse
    , ReplaceAllShapesWithSheetsChartResponse
    , replaceAllShapesWithSheetsChartResponse
    , raswscrOccurrencesChanged

    -- * CreateTableRequest
    , CreateTableRequest
    , createTableRequest
    , ctrObjectId
    , ctrRows
    , ctrElementProperties
    , ctrColumns

    -- * TableBOrderProperties
    , TableBOrderProperties
    , tableBOrderProperties
    , tbopTableBOrderFill
    , tbopWeight
    , tbopDashStyle

    -- * Response
    , Response
    , response
    , rReplaceAllShapesWithImage
    , rCreateLine
    , rReplaceAllText
    , rReplaceAllShapesWithSheetsChart
    , rCreateShape
    , rGroupObjects
    , rCreateSheetsChart
    , rDuplicateObject
    , rCreateTable
    , rCreateVideo
    , rCreateImage
    , rCreateSlide

    -- * OptionalColor
    , OptionalColor
    , optionalColor
    , ocOpaqueColor

    -- * DuplicateObjectRequest
    , DuplicateObjectRequest
    , duplicateObjectRequest
    , dorObjectId
    , dorObjectIds

    -- * PlaceholderType
    , PlaceholderType (..)

    -- * UnGroupObjectsRequest
    , UnGroupObjectsRequest
    , unGroupObjectsRequest
    , ugorObjectIds

    -- * Page
    , Page
    , page
    , pNotesProperties
    , pMasterProperties
    , pObjectId
    , pPageElements
    , pSlideProperties
    , pPageProperties
    , pLayoutProperties
    , pPageType
    , pRevisionId

    -- * ReplaceAllTextResponse
    , ReplaceAllTextResponse
    , replaceAllTextResponse
    , ratrOccurrencesChanged

    -- * ReplaceAllShapesWithImageRequestReplaceMethod
    , ReplaceAllShapesWithImageRequestReplaceMethod (..)

    -- * Bullet
    , Bullet
    , bullet
    , bGlyph
    , bListId
    , bNestingLevel
    , bBulletStyle

    -- * CreateSheetsChartRequestLinkingMode
    , CreateSheetsChartRequestLinkingMode (..)

    -- * UpdateImagePropertiesRequest
    , UpdateImagePropertiesRequest
    , updateImagePropertiesRequest
    , uiprObjectId
    , uiprImageProperties
    , uiprFields

    -- * SlideProperties
    , SlideProperties
    , slideProperties
    , spLayoutObjectId
    , spMasterObjectId
    , spNotesPage

    -- * Presentation
    , Presentation
    , presentation
    , preSlides
    , preNotesMaster
    , preMasters
    , preLocale
    , prePresentationId
    , preTitle
    , preRevisionId
    , prePageSize
    , preLayouts

    -- * ThemeColorPair
    , ThemeColorPair
    , themeColorPair
    , tcpColor
    , tcpType

    -- * ShapeBackgRoundFillPropertyState
    , ShapeBackgRoundFillPropertyState (..)

    -- * OpaqueColorThemeColor
    , OpaqueColorThemeColor (..)

    -- * Shadow
    , Shadow
    , shadow
    , sTransform
    , sColor
    , sBlurRadius
    , sRotateWithShape
    , sAlpha
    , sAlignment
    , sPropertyState
    , sType

    -- * LinePropertiesDashStyle
    , LinePropertiesDashStyle (..)

    -- * ImageProperties
    , ImageProperties
    , imageProperties
    , ipCropProperties
    , ipLink
    , ipTransparency
    , ipShadow
    , ipContrast
    , ipRecolor
    , ipOutline
    , ipBrightness

    -- * ParagraphStyleAlignment
    , ParagraphStyleAlignment (..)

    -- * Line
    , Line
    , line
    , lLineProperties
    , lLineType

    -- * CreateVideoResponse
    , CreateVideoResponse
    , createVideoResponse
    , cvrObjectId

    -- * UpdateTableBOrderPropertiesRequestBOrderPosition
    , UpdateTableBOrderPropertiesRequestBOrderPosition (..)

    -- * LayoutReference
    , LayoutReference
    , layoutReference
    , lrPredefinedLayout
    , lrLayoutId

    -- * LineFill
    , LineFill
    , lineFill
    , lfSolidFill

    -- * UpdatePageElementTransformRequest
    , UpdatePageElementTransformRequest
    , updatePageElementTransformRequest
    , upetrTransform
    , upetrObjectId
    , upetrApplyMode

    -- * ShadowAlignment
    , ShadowAlignment (..)

    -- * InsertTableRowsRequest
    , InsertTableRowsRequest
    , insertTableRowsRequest
    , itrrInsertBelow
    , itrrNumber
    , itrrCellLocation
    , itrrTableObjectId

    -- * ReplaceAllShapesWithSheetsChartRequestLinkingMode
    , ReplaceAllShapesWithSheetsChartRequestLinkingMode (..)

    -- * UnmergeTableCellsRequest
    , UnmergeTableCellsRequest
    , unmergeTableCellsRequest
    , utcrObjectId
    , utcrTableRange

    -- * LinePropertiesEndArrow
    , LinePropertiesEndArrow (..)

    -- * Video
    , Video
    , video
    , vURL
    , vSource
    , vId
    , vVideoProperties

    -- * TableCellBackgRoundFillPropertyState
    , TableCellBackgRoundFillPropertyState (..)

    -- * CreateVideoRequestSource
    , CreateVideoRequestSource (..)

    -- * UpdateTableColumnPropertiesRequest
    , UpdateTableColumnPropertiesRequest
    , updateTableColumnPropertiesRequest
    , utcprObjectId
    , utcprTableColumnProperties
    , utcprFields
    , utcprColumnIndices

    -- * TableCellProperties
    , TableCellProperties
    , tableCellProperties
    , tcpTableCellBackgRoundFill
    , tcpContentAlignment

    -- * CreateLineResponse
    , CreateLineResponse
    , createLineResponse
    , clrObjectId

    -- * WordArt
    , WordArt
    , wordArt
    , waRenderedText

    -- * TableCellBackgRoundFill
    , TableCellBackgRoundFill
    , tableCellBackgRoundFill
    , tcbrfSolidFill
    , tcbrfPropertyState

    -- * LinkRelativeLink
    , LinkRelativeLink (..)

    -- * VideoSource
    , VideoSource (..)

    -- * TextRun
    , TextRun
    , textRun
    , trStyle
    , trContent

    -- * RefreshSheetsChartRequest
    , RefreshSheetsChartRequest
    , refreshSheetsChartRequest
    , rscrObjectId

    -- * TableRow
    , TableRow
    , tableRow
    , trTableRowProperties
    , trTableCells
    , trRowHeight

    -- * WeightedFontFamily
    , WeightedFontFamily
    , weightedFontFamily
    , wffFontFamily
    , wffWeight

    -- * CreateVideoRequest
    , CreateVideoRequest
    , createVideoRequest
    , creObjectId
    , creElementProperties
    , creSource
    , creId

    -- * TextContent
    , TextContent
    , textContent
    , tcTextElements
    , tcLists

    -- * Shape
    , Shape
    , shape
    , sShapeType
    , sText
    , sPlaceholder
    , sShapeProperties

    -- * AffineTransform
    , AffineTransform
    , affineTransform
    , atTranslateX
    , atShearY
    , atTranslateY
    , atShearX
    , atScaleX
    , atUnit
    , atScaleY

    -- * CreateSheetsChartResponse
    , CreateSheetsChartResponse
    , createSheetsChartResponse
    , cscrsObjectId

    -- * Range
    , Range
    , range
    , rEndIndex
    , rType
    , rStartIndex

    -- * CreateImageRequest
    , CreateImageRequest
    , createImageRequest
    , cirObjectId
    , cirURL
    , cirElementProperties

    -- * MergeTableCellsRequest
    , MergeTableCellsRequest
    , mergeTableCellsRequest
    , mtcrObjectId
    , mtcrTableRange

    -- * Xgafv
    , Xgafv (..)

    -- * AutoTextType
    , AutoTextType (..)

    -- * WriteControl
    , WriteControl
    , writeControl
    , wcRequiredRevisionId

    -- * TextStyle
    , TextStyle
    , textStyle
    , tsFontFamily
    , tsLink
    , tsBackgRoundColor
    , tsBaselineOffSet
    , tsForegRoundColor
    , tsFontSize
    , tsSmallCaps
    , tsUnderline
    , tsWeightedFontFamily
    , tsItalic
    , tsBold
    , tsStrikethrough

    -- * SolidFill
    , SolidFill
    , solidFill
    , sfColor
    , sfAlpha

    -- * UpdateTextStyleRequest
    , UpdateTextStyleRequest
    , updateTextStyleRequest
    , utsrStyle
    , utsrTextRange
    , utsrObjectId
    , utsrCellLocation
    , utsrFields

    -- * Recolor
    , Recolor
    , recolor
    , rName
    , rRecolorStops

    -- * PageProperties
    , PageProperties
    , pageProperties
    , ppPageBackgRoundFill
    , ppColorScheme

    -- * PageBackgRoundFill
    , PageBackgRoundFill
    , pageBackgRoundFill
    , pbrfStretchedPictureFill
    , pbrfSolidFill
    , pbrfPropertyState

    -- * NestingLevel
    , NestingLevel
    , nestingLevel
    , nlBulletStyle

    -- * OpaqueColor
    , OpaqueColor
    , opaqueColor
    , ocThemeColor
    , ocRgbColor

    -- * TableBOrderPropertiesDashStyle
    , TableBOrderPropertiesDashStyle (..)

    -- * CreateSlideRequest
    , CreateSlideRequest
    , createSlideRequest
    , csrsObjectId
    , csrsSlideLayoutReference
    , csrsInsertionIndex
    , csrsPlaceholderIdMAppings

    -- * TableCellLocation
    , TableCellLocation
    , tableCellLocation
    , tclColumnIndex
    , tclRowIndex

    -- * UpdateSlidesPositionRequest
    , UpdateSlidesPositionRequest
    , updateSlidesPositionRequest
    , usprSlideObjectIds
    , usprInsertionIndex

    -- * ReplaceAllShapesWithImageRequest
    , ReplaceAllShapesWithImageRequest
    , replaceAllShapesWithImageRequest
    , raswirImageReplaceMethod
    , raswirPageObjectIds
    , raswirContainsText
    , raswirImageURL
    , raswirReplaceMethod

    -- * PageElement
    , PageElement
    , pageElement
    , peTransform
    , peImage
    , peSize
    , peSheetsChart
    , peObjectId
    , peLine
    , peElementGroup
    , peVideo
    , peWordArt
    , peShape
    , peTitle
    , peTable
    , peDescription

    -- * ColorStop
    , ColorStop
    , colorStop
    , csColor
    , csAlpha
    , csPosition

    -- * DeleteObjectRequest
    , DeleteObjectRequest
    , deleteObjectRequest
    , dObjectId

    -- * CreateSlideResponse
    , CreateSlideResponse
    , createSlideResponse
    , ccObjectId

    -- * ColorScheme
    , ColorScheme
    , colorScheme
    , csColors

    -- * TableCell
    , TableCell
    , tableCell
    , tcColumnSpan
    , tcLocation
    , tcText
    , tcRowSpan
    , tcTableCellProperties

    -- * ListNestingLevel
    , ListNestingLevel
    , listNestingLevel
    , lnlAddtional

    -- * Outline
    , Outline
    , outline
    , oOutlineFill
    , oWeight
    , oDashStyle
    , oPropertyState

    -- * UpdateVideoPropertiesRequest
    , UpdateVideoPropertiesRequest
    , updateVideoPropertiesRequest
    , uvprObjectId
    , uvprVideoProperties
    , uvprFields

    -- * VideoProperties
    , VideoProperties
    , videoProperties
    , vpStart
    , vpAutoPlay
    , vpMute
    , vpEnd
    , vpOutline

    -- * LayoutPlaceholderIdMApping
    , LayoutPlaceholderIdMApping
    , layoutPlaceholderIdMApping
    , lpimaObjectId
    , lpimaLayoutPlaceholderObjectId
    , lpimaLayoutPlaceholder

    -- * LineLineType
    , LineLineType (..)

    -- * PagePageType
    , PagePageType (..)

    -- * CreateImageResponse
    , CreateImageResponse
    , createImageResponse
    , ciriObjectId

    -- * TextElement
    , TextElement
    , textElement
    , teParagraphMarker
    , teAutoText
    , teEndIndex
    , teTextRun
    , teStartIndex

    -- * ParagraphStyleSpacingMode
    , ParagraphStyleSpacingMode (..)

    -- * DeleteParagraphBulletsRequest
    , DeleteParagraphBulletsRequest
    , deleteParagraphBulletsRequest
    , dpbrTextRange
    , dpbrObjectId
    , dpbrCellLocation

    -- * InsertTextRequest
    , InsertTextRequest
    , insertTextRequest
    , itrText
    , itrObjectId
    , itrInsertionIndex
    , itrCellLocation

    -- * UpdateTableBOrderPropertiesRequest
    , UpdateTableBOrderPropertiesRequest
    , updateTableBOrderPropertiesRequest
    , utboprBOrderPosition
    , utboprObjectId
    , utboprTableBOrderProperties
    , utboprTableRange
    , utboprFields

    -- * CreateLineRequest
    , CreateLineRequest
    , createLineRequest
    , clrlObjectId
    , clrlLineCategory
    , clrlElementProperties

    -- * Placeholder
    , Placeholder
    , placeholder
    , pParentObjectId
    , pType
    , pIndex

    -- * LayoutProperties
    , LayoutProperties
    , layoutProperties
    , lpMasterObjectId
    , lpName
    , lpDisplayName

    -- * UpdateShapePropertiesRequest
    , UpdateShapePropertiesRequest
    , updateShapePropertiesRequest
    , usprObjectId
    , usprShapeProperties
    , usprFields

    -- * Table
    , Table
    , table
    , tTableRows
    , tVerticalBOrderRows
    , tRows
    , tColumns
    , tHorizontalBOrderRows
    , tTableColumns

    -- * ThemeColorPairType
    , ThemeColorPairType (..)

    -- * LinePropertiesStartArrow
    , LinePropertiesStartArrow (..)

    -- * ShapeProperties
    , ShapeProperties
    , shapeProperties
    , spLink
    , spShadow
    , spOutline
    , spContentAlignment
    , spShapeBackgRoundFill

    -- * ShadowPropertyState
    , ShadowPropertyState (..)

    -- * ShadowType
    , ShadowType (..)

    -- * ShapeBackgRoundFill
    , ShapeBackgRoundFill
    , shapeBackgRoundFill
    , sbrfSolidFill
    , sbrfPropertyState

    -- * UpdatePageElementTransformRequestApplyMode
    , UpdatePageElementTransformRequestApplyMode (..)

    -- * CreateParagraphBulletsRequest
    , CreateParagraphBulletsRequest
    , createParagraphBulletsRequest
    , cpbrTextRange
    , cpbrObjectId
    , cpbrBulletPreset
    , cpbrCellLocation

    -- * DimensionUnit
    , DimensionUnit (..)

    -- * ReplaceImageRequestImageReplaceMethod
    , ReplaceImageRequestImageReplaceMethod (..)

    -- * OutlineDashStyle
    , OutlineDashStyle (..)

    -- * AffineTransformUnit
    , AffineTransformUnit (..)

    -- * UpdatePageElementAltTextRequest
    , UpdatePageElementAltTextRequest
    , updatePageElementAltTextRequest
    , upeatrObjectId
    , upeatrTitle
    , upeatrDescription

    -- * UpdateTableCellPropertiesRequest
    , UpdateTableCellPropertiesRequest
    , updateTableCellPropertiesRequest
    , uObjectId
    , uTableCellProperties
    , uTableRange
    , uFields

    -- * ReplaceAllShapesWithImageRequestImageReplaceMethod
    , ReplaceAllShapesWithImageRequestImageReplaceMethod (..)

    -- * SheetsChartProperties
    , SheetsChartProperties
    , sheetsChartProperties
    , scpChartImageProperties

    -- * ParagraphStyle
    , ParagraphStyle
    , paragraphStyle
    , psLineSpacing
    , psDirection
    , psIndentFirstLine
    , psIndentEnd
    , psIndentStart
    , psAlignment
    , psSpaceBelow
    , psSpacingMode
    , psSpaceAbove

    -- * CreateTableResponse
    , CreateTableResponse
    , createTableResponse
    , ctrtObjectId

    -- * TableColumnProperties
    , TableColumnProperties
    , tableColumnProperties
    , tcpColumnWidth

    -- * DuplicateObjectResponse
    , DuplicateObjectResponse
    , duplicateObjectResponse
    , dupObjectId

    -- * UpdateParagraphStyleRequest
    , UpdateParagraphStyleRequest
    , updateParagraphStyleRequest
    , upsrStyle
    , upsrTextRange
    , upsrObjectId
    , upsrCellLocation
    , upsrFields

    -- * ReplaceAllTextRequest
    , ReplaceAllTextRequest
    , replaceAllTextRequest
    , ratrPageObjectIds
    , ratrContainsText
    , ratrReplaceText

    -- * TableRange
    , TableRange
    , tableRange
    , trColumnSpan
    , trLocation
    , trRowSpan

    -- * Request'
    , Request'
    , request'
    , reqReplaceAllShapesWithImage
    , reqDeleteObject
    , reqUpdateSlidesPosition
    , reqUpdateShapeProperties
    , reqCreateParagraphBullets
    , reqCreateLine
    , reqInsertText
    , reqUpdateTableBOrderProperties
    , reqDeleteParagraphBullets
    , reqDeleteTableRow
    , reqUpdateTableCellProperties
    , reqReplaceAllText
    , reqUpdatePageElementAltText
    , reqUpdateParagraphStyle
    , reqReplaceImage
    , reqReplaceAllShapesWithSheetsChart
    , reqCreateShape
    , reqUpdatePageProperties
    , reqUpdateLineProperties
    , reqDeleteTableColumn
    , reqGroupObjects
    , reqDeleteText
    , reqUpdateTableRowProperties
    , reqCreateSheetsChart
    , reqInsertTableColumns
    , reqUpdateImageProperties
    , reqUnGroupObjects
    , reqDuplicateObject
    , reqCreateTable
    , reqCreateVideo
    , reqRefreshSheetsChart
    , reqUpdateTableColumnProperties
    , reqUnmergeTableCells
    , reqUpdatePageElementTransform
    , reqInsertTableRows
    , reqCreateImage
    , reqMergeTableCells
    , reqCreateSlide
    , reqUpdateTextStyle
    , reqUpdateVideoProperties

    -- * SubstringMatchCriteria
    , SubstringMatchCriteria
    , substringMatchCriteria
    , smcMatchCase
    , smcText
    ) where

import Network.Google.Prelude
import Network.Google.Slides.Types.Product
import Network.Google.Slides.Types.Sum

-- | Default request referring to version 'v1' of the Google Slides API. This contains the host and root path used as a starting point for constructing service requests.
slidesService :: ServiceConfig
slidesService
  = defaultService (ServiceId "slides:v1")
      "slides.googleapis.com"

-- | View your Google Spreadsheets
spreadsheetsReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/spreadsheets.readonly"]
spreadsheetsReadOnlyScope = Proxy;

-- | View and manage your Google Slides presentations
presentationsScope :: Proxy '["https://www.googleapis.com/auth/presentations"]
presentationsScope = Proxy;

-- | View the files in your Google Drive
driveReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/drive.readonly"]
driveReadOnlyScope = Proxy;

-- | View and manage the files in your Google Drive
driveScope :: Proxy '["https://www.googleapis.com/auth/drive"]
driveScope = Proxy;

-- | View and manage Google Drive files and folders that you have opened or
-- created with this app
driveFileScope :: Proxy '["https://www.googleapis.com/auth/drive.file"]
driveFileScope = Proxy;

-- | View and manage your spreadsheets in Google Drive
spreadsheetsScope :: Proxy '["https://www.googleapis.com/auth/spreadsheets"]
spreadsheetsScope = Proxy;

-- | View your Google Slides presentations
presentationsReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/presentations.readonly"]
presentationsReadOnlyScope = Proxy;
