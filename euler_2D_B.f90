program euler_2D_B
    use params
    use globals
    use interface_mod

    implicit none
    integer :: n

    !time.txtをフォーマット
    open(50, file = 'time.txt')
    write(50, *) 'filename,     n,      time'
    write(50, *)
    write(50, *)
    close(50)

    !計算開始
    call alloc(1)
    call glid
    call init
    call metrics
    !時間進める
    res_x(:, :) = 1.0d0
    res_y(:, :) = 1.0d0
    do while (n <= nmax .or. res_x(30, 1) >= 5.0d0 * 10.0d0 ** (-3.0d0))
        call bound
        call cflc
        call integ
        call calc_res
        call writed(n)
        !write (*, *) 'dt = ', dt
        n = n + 1
    end do
    write(*,'(a9, i3)') 'ntime = ', n - 1

    call alloc(0)

end program euler_2D_B