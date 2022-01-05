
module modq_dictionary
    use modq_base_types

    type, private :: LinkedList
        type(Link), pointer :: head
        type(Link), pointer :: tail
        integer :: size

    contains
        procedure, public :: get => list_get
        procedure, public :: append => list_append
        procedure, public :: delete => list_delete
    end type LinkedList


    type, private :: KeyValuePair
        class(HashableObject), allocatable :: key
        class(*), allocatable :: value
    end type KeyValuePair

    interface KeyValuePair
        module procedure initialize_keyvaluepair
    end interface KeyValuePair


    type, private :: Link
        type(KeyValuePair) :: pair
        type(Link), pointer :: next
    end type Link


    type :: Dict
        type(LinkedList), allocatable :: divisions(:)
        integer :: size

    contains
        procedure, public :: get => dict_get
        procedure, public :: has => dict_has
        procedure, public :: add => dict_add
        procedure, public :: delete => dict_delete
    end type Dict

    interface Dict
        module procedure initialize_dict
    end interface Dict


    type, extends(HashableObject) :: IntObject
        integer :: value
    contains
        procedure, public :: equals_obj => int__equals_obj
        procedure, public :: hash => int__hash
    end type IntObject

contains

    type(KeyValuePair) function initialize_keyvaluepair(key, value) result(pair)
        class(HashableObject), intent(in) :: key
        class(*), intent(in) :: value

        pair = KeyValuePair(null(), null())

        pair%key = key
        pair%value = value
    end function initialize_keyvaluepair


    function list_get(self, key) result(obj)
        class(LinkedList), target, intent(in) :: self
        class(ComparableObject), intent(in) :: key
        class(*), pointer :: obj
        type(Link), pointer :: next_link

        class(HashableObject), pointer :: pair

        integer :: link_idx

        obj => null()
        next_link => self%head

        do while (associated(next_link))
            idx = idx - 1

            if (next_link%pair%key%equals_obj(key)) then
                obj => next_link%pair%value
                exit
            end if

            next_link => next_link%next
        end do
    end function list_get


    subroutine list_append(self, pair)
        class(LinkedList), intent(inout) :: self
        type(KeyValuePair), intent(in) :: pair

        type(Link), allocatable :: link_target
        type(Link), pointer :: new_link

        link_target = Link(pair, null())
        allocate(new_link, source=link_target)

        if (self%size == 0) then
            self%head => new_link
            self%tail => new_link
        else
            self%tail%next => new_link
            self%tail => new_link
        end if

        self%size = self%size + 1
    end subroutine list_append


    subroutine list_delete(self)
        class(LinkedList), intent(inout) :: self
        type(Link), pointer :: current_link
        type(Link), pointer :: next_link

        current_link => self%head
        next_link => null()

        do while (associated(current_link))
            next_link => current_link%next
            deallocate(current_link)
            current_link => next_link
        end do
    end subroutine list_delete


    type(Dict) function initialize_dict() result(dictionary)
        integer :: idx

        dictionary = Dict(null(), 0)

        allocate(dictionary%divisions(128))

        do idx = 1, 128
            dictionary%divisions(idx) = LinkedList(null(), null(), 0)
        end do

    end function initialize_dict


    function dict_get(self, key) result(value)
        class(Dict), target, intent(in) :: self
        class(HashableObject), intent(in) :: key
        class(*), pointer :: value

        ! Get value for key
        value => self%divisions(mod(key%hash(), 128))%get(key)

    end function dict_get


    subroutine dict_add(self, key, value)
        class(Dict), intent(inout) :: self
        class(HashableObject), intent(in) :: key
        class(*), intent(in) :: value

        if (.not. self%has(key)) then
            call self%divisions(mod(key%hash(), 128))%append(KeyValuePair(key, value))
        else
            call bort("Key already exists in dictionary.")
        end if
    end subroutine dict_add


    function dict_has(self, key) result(has)
        class(Dict), intent(in) :: self
        class(HashableObject), intent(in) :: key
        logical :: has

        ! Check if key has value
        has = associated(self%divisions(mod(key%hash(), 128))%get(key))
    end function dict_has


    subroutine dict_delete(self)
        class(Dict), intent(inout) :: self

        integer :: idx

        ! Delete value for key
        do idx = 1, size(self%divisions)
            call self%divisions(idx)%delete()
        end do
    end subroutine dict_delete


    logical function int__equals_obj(self, other) result(equals)
        class(IntObject), intent(in) :: self
        class(ComparableObject), intent(in) :: other

        select type(other)
        type is (IntObject)
            equals = (self%value == other%value)
        class default
            call bort("Wrong type found comparing integers.")
        end select
    end function int__equals_obj


    integer function int__hash(self) result(hash)
        class(IntObject), intent(in) :: self

        hash = self%value
    end function int__hash
end module modq_dictionary
