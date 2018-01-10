module point_m

  implicit none

  interface point
     ! The difference between the procedures is the rank and type of
     ! the first argument.
     module procedure point_2, point_2_dble, point_3, point_3_dble, point_4, &
          point_4_dble
  end interface

  private
  public point

contains

  real function point_2(array, location)

    use nr_util, only: assert

    real, intent(in):: array(:, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 2, "point_2")

    point_2 = array(location(1), location(2))

  end function point_2

  !***************************************************

  double precision function point_2_dble(array, location)

    use nr_util, only: assert

    double precision, intent(in):: array(:, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 2, "point_2_dble")

    point_2_dble = array(location(1), location(2))

  end function point_2_dble

  !***************************************************

  real function point_3(array, location)

    use nr_util, only: assert

    real, intent(in):: array(:, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 3, "point_3")

    point_3 = array(location(1), location(2), location(3))

  end function point_3

  !***************************************************

  double precision function point_3_dble(array, location)

    use nr_util, only: assert

    double precision, intent(in):: array(:, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 3, "point_3_dble")

    point_3_dble = array(location(1), location(2), location(3))

  end function point_3_dble

  !***************************************************

  real function point_4(array, location)

    use nr_util, only: assert

    real, intent(in):: array(:, :, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 4, "point_4")

    point_4 = array(location(1), location(2), location(3), location(4))

  end function point_4

  !***************************************************

  double precision function point_4_dble(array, location)

    use nr_util, only: assert

    double precision, intent(in):: array(:, :, :, :)
    integer, intent(in):: location(:)

    !---------------------------------

    call assert(size(location) == 4, "point_4_dble")

    point_4_dble = array(location(1), location(2), location(3), location(4))

  end function point_4_dble

end module point_m
