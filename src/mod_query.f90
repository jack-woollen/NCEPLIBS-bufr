module mod_query
  use moda_tables
  use moda_msgcwd
  use moda_usrint

  implicit none

  character(len=3), parameter :: Subset = 'SUB'
  character(len=3), parameter :: DelayedRep = 'DRP'
  character(len=3), parameter :: Sequence = 'SEQ'
  character(len=3), parameter :: FixedRep = 'REP'
  character(len=3), parameter :: Value = 'REP'

  private
  public::query

contains
  subroutine query(lunit, query_str, data_ptr)
    integer, intent(in) :: lunit
    character(len=*), intent(in) :: query_str
    real(kind=8), pointer, intent(out) :: data_ptr(:, :)

    integer :: lun, il, im

    character(len=10), allocatable :: mnemonics(:)
    integer, allocatable :: table_path(:)
    integer, allocatable :: target_nodes(:)
    real(kind=8), target, allocatable :: data(:, :)

    call status(lunit, lun, il, im)

    call find_target_nodes(lun, query_str, target_nodes)
    call collect_data(lun, target_nodes, data)

    data_ptr => data

    deallocate(target_nodes)

  end subroutine query


  subroutine split_query_str(query_str, mnemonics)
    character, parameter :: Delimiter = "/"

    character(len=*), intent(in) :: query_str
    character(len=10), allocatable, intent(out) :: mnemonics(:)

    integer(kind=8), allocatable :: slash_positions(:)
    integer(kind=8) :: slash_idx, char_idx
    integer(kind=8) :: num_seqs
    integer(kind=8) :: mnemonic_start_pos, mnemonic_end_pos

    allocate(slash_positions(count(transfer(query_str, "a", len(query_str)) == Delimiter)))

    ! Find the index of the delimiters.
    ! NOTE: Intel version of findloc has a bug, so avoid it
    slash_idx = 1
    do char_idx = 1, len(query_str)
      !ignore optional leading slash
      if (query_str(char_idx:char_idx) == Delimiter) then
        slash_positions(slash_idx) = char_idx
        slash_idx = slash_idx + 1
      end if
    end do

    ! Capture the sequence mnemonic strings
    allocate(mnemonics(size(slash_positions)))
    do slash_idx = 1, size(slash_positions) - 1
      mnemonics(slash_idx) = trim(query_str(slash_positions(slash_idx) + 1 : slash_positions(slash_idx + 1) - 1))
    end do

    ! Capture the data mnemonic
    mnemonics(size(slash_positions)) = trim(query_str(slash_positions(size(slash_positions)) + 1 : len(query_str)))

  end subroutine split_query_str


  subroutine find_target_nodes(lun, query_str, target_nodes)
    integer, intent(in) :: lun
    character(len=*), intent(in) :: query_str
    integer, intent(out), allocatable :: target_nodes(:)

    integer :: current_sequence
    integer :: node_idx
    integer :: table_cursor, mnemonic_cursor
    character(len=10), allocatable :: mnemonics(:)

    integer, allocatable :: branches(:)

    call split_query_str(query_str, mnemonics)

    allocate(branches(size(mnemonics) - 1))
    allocate(target_nodes(0))

    node_idx = 1
    table_cursor = 0
    mnemonic_cursor = 0
    do node_idx = inode(lun), isc(inode(lun))
      if (mnemonic_cursor == size(mnemonics) - 1 .and. &
          table_cursor == mnemonic_cursor .and. &
          tag(node_idx) == mnemonics(size(mnemonics))) then

        ! We found a target
        target_nodes = [target_nodes, node_idx]

      else if (typ(node_idx) == DelayedRep .or. typ(node_idx) == FixedRep) then
        ! Enter the sequence
        if (tag(node_idx + 1) == mnemonics(mnemonic_cursor + 1) .and. &
            table_cursor == mnemonic_cursor) then

          mnemonic_cursor = mnemonic_cursor + 1
          branches(mnemonic_cursor) = node_idx
        end if

        current_sequence = node_idx
        table_cursor = table_cursor + 1

      else if (link(current_sequence) == node_idx) then
        ! Exit sequence
        if (mnemonic_cursor > 0 .and. &
            table_cursor == mnemonic_cursor .and. &
            link(branches(mnemonic_cursor)) == node_idx) then

          mnemonic_cursor = mnemonic_cursor - 1
        end if

        table_cursor = table_cursor - 1
        current_sequence = jmpb(node_idx - 1)
      end if
    end do

    deallocate(mnemonics)

    if (size(target_nodes) == 0) then
      error stop 'Could not find the target node for ' // query_str // '.'
    end if
  end subroutine


  subroutine collect_data(lun, target_nodes, dat)
    integer, intent(in) :: lun
    integer, intent(in) :: target_nodes(:)
    real(kind=8), intent(out), allocatable :: dat(:, :)

    integer :: dims(2)
    integer :: col_idx
    integer :: data_cursor
    integer, allocatable :: collected_data_cursor(:)

    dims = result_shape(lun, target_nodes)
    allocate(dat(dims(1), dims(2)))

    allocate(collected_data_cursor(dims(2)))
    collected_data_cursor = 1
    do data_cursor = 1, nval(lun)
      do col_idx = 1, dims(2)
        if (inv(data_cursor, lun) == target_nodes(col_idx)) then
          dat(collected_data_cursor(col_idx), col_idx) = val(data_cursor, lun)
          collected_data_cursor(col_idx) = collected_data_cursor(col_idx) + 1
        end if
      end do
    end do
  end subroutine


  function result_shape(lun, target_nodes) result(dims)
    integer, intent(in) :: lun
    integer, intent(in) :: target_nodes(:)

    integer :: dims(2)
    integer :: data_cursor

    dims = 0
    do data_cursor = 1, nval(lun)
      if (inv(data_cursor, lun) == target_nodes(1)) then
        dims(1) = dims(1) + 1
      end if
    end do

    dims(2) = size(target_nodes)
  end function

end module mod_query