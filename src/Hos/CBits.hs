module Hos.CBits where
    import Data.Word

    import Foreign.Ptr

    foreign import ccall "rts_integration.h ext_page_aligned_alloc" cPageAlignedAlloc :: Word64 -> IO Word64
    foreign import ccall "allocator.h alloc_from_regions" cPageAlignedPhysAlloc :: Word64 -> IO Word64
    foreign import ccall "rts_integration.h arch_invalidate_page" archInvalidatePage :: Word64 -> IO ()
    foreign import ccall "rts_integration.h ext_halt" cHalt :: IO ()


    foreign import ccall "rts_integration.h ptrToWord" ptrToWord :: Ptr a -> Word64

    foreign import ccall "rts_integration.h wordToPtr" wordToPtr :: Word64 -> Ptr a
