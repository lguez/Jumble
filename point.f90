module point_m

  use nr_util, only: assert

  implicit none

  interface point
     ! Returns the element of an array, given the vector of its
     ! indices. That is, you can write:

     ! point(array, location)

     ! instead of :

     ! array(location(1), location(2), location(3))

     ! for example. The difference between the procedures is the rank
     ! and type of the first argument.
     module procedure point_1, point_1_dble, point_2, point_2_dble, point_3, &
          point_3_dble, point_4, point_4_dble
  end interface point

  private
  public point

contains

  real function point_1(array, location)

    real, intent(in):: array(:)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 1, "point_1")

    point_1 = array(location(1))

  end function point_1

  !***************************************************

  double precision function point_1_dble(array, location)

    double precision, intent(in):: array(:)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 1, "point_1_dble")

    point_1_dble = array(location(1))

  end function point_1_dble

  !***************************************************

  real function point_2(array, location)

    real, intent(in):: array(:, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 2, "point_2")

    point_2 = array(location(1), location(2))

  end function point_2

  !***************************************************

  double precision function point_2_dble(array, location)

    double precision, intent(in):: array(:, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 2, "point_2_dble")

    point_2_dble = array(location(1), location(2))

  end function point_2_dble

  !***************************************************

  real function point_3(array, location)

    real, intent(in):: array(:, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 3, "point_3")

    point_3 = array(location(1), location(2), location(3))

  end function point_3

  !***************************************************

  double precision function point_3_dble(array, location)

    double precision, intent(in):: array(:, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 3, "point_3_dble")

    point_3_dble = array(location(1), location(2), location(3))

  end function point_3_dble

  !***************************************************

  real function point_4(array, location)

    real, intent(in):: array(:, :, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 4, "point_4")

    point_4 = array(location(1), location(2), location(3), location(4))

  end function point_4

  !***************************************************

  double precision function point_4_dble(array, location)

    double precision, intent(in):: array(:, :, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 4, "point_4_dble")

    point_4_dble = array(location(1), location(2), location(3), location(4))

  end function point_4_dble

end module point_m
