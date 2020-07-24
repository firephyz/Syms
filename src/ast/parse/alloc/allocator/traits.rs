///////////////////////////////////////////////////////////////////////////////
// Allocator trait that implements allocating general objects.
// Allocates Object handles that originate from Alloc.
///////////////////////////////////////////////////////////////////////////////
pub(super) trait Allocator<'a, Obj>
  where Obj: Allocable + ?Sized
{
    //type InitData = <Object as Allocable>::InitData;
    //type Handle = AllocHandle<'h, Object, Alloc>;
    type Object: Allocable;
    type Handle<'h>;//: AllocHandleTrait<Self::Object::Allocator>;
    type Error;

    fn allocate(&'a mut self, data: <Self::Object as Allocable>::InitData<'_>) -> Result<Self::Handle<'a>, Self::Error>;
    fn deallocate(&mut self, item: Self::Handle<'a>);
}

// Allocators that actually own the resources
pub(super) trait PrimAllocator<'a, Obj> : Allocator<'a, Obj>
  where Obj: Allocable + ?Sized
{
    type Object: Allocable;
    type Handle<'h>;

    fn get(&self, index: usize) -> &AllocObject<Self::Object>;
}

///////////////////////////////////////////////////////////////////////////////
// Reference counted objects
///////////////////////////////////////////////////////////////////////////////
pub(super) trait RefCount {
    fn inc_ref(&self);
    fn dec_ref(&self);
    fn ref_count(&self) -> u32;
}

///////////////////////////////////////////////////////////////////////////////
// Types allocable by a impl PrimAllocator
///////////////////////////////////////////////////////////////////////////////
pub(in crate::ast::parse::alloc) trait Allocable {
    type Alloc<'a>: PrimAllocator<'a, Self>;
    type InitData<'a>;

    fn init(data: Self::InitData<'_>) -> Self where Self: Sized;
}
