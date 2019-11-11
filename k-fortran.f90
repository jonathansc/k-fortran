module lib
    implicit None

contains
    subroutine get_arguments(n_iterations, filename_input, n_samples, n_clusters, n_features)
        character(len = :), allocatable, intent(out) :: filename_input
        integer, intent(out) :: n_iterations, n_samples, n_features, n_clusters
        character(len = :), allocatable :: iterations_char, clusters_char, samples_char, features_char
        integer :: arg_length

        if (command_argument_count().ne.5) then
            print *, "Usage: k-fortran.f90&
                <number of iterations> <input file> <number of samples> <number of clusters> <number of features>"
            stop
        end if

        call get_command_argument(number = 1, length = arg_length)
        allocate (character(len = arg_length) :: iterations_char)
        call get_command_argument(number = 1, value = iterations_char)

        call get_command_argument(number = 2, length = arg_length)
        allocate (character(len = arg_length) :: filename_input)
        call get_command_argument(number = 2, value = filename_input)

        call get_command_argument(number = 3, length = arg_length)
        allocate (character(len = arg_length) :: samples_char)
        call get_command_argument(number = 3, value = samples_char)

        call get_command_argument(number = 4, length = arg_length)
        allocate (character(len = arg_length) :: clusters_char)
        call get_command_argument(number = 4, value = clusters_char)

        call get_command_argument(number = 5, length = arg_length)
        allocate (character(len = arg_length) :: features_char)
        call get_command_argument(number = 5, value = features_char)

        read(iterations_char, *) n_iterations
        read(samples_char, *) n_samples
        read(clusters_char, *) n_clusters
        read(features_char, *) n_features
    end subroutine get_arguments

    subroutine read_samples(filename_input, samples, assignments, n_samples, n_features)
        character(len = :), allocatable, intent(in) :: filename_input
        integer, intent(in) :: n_samples, n_features
        real, dimension(:, :), allocatable, intent(out) :: samples
        integer, dimension(:), allocatable, intent(out) :: assignments
        integer :: current_line

        allocate(samples(n_samples, n_features))
        allocate(assignments(n_samples))

        open(unit = 11, file = filename_input, status = "old")
        do current_line = 1, n_samples
            read(11, *) samples(current_line, :)
        end do
        close(11)
    end subroutine read_samples

    subroutine initialize_centroids(samples, centroids, n_samples, n_clusters, n_features)
        integer, intent(in) :: n_samples, n_clusters, n_features
        real, dimension(:, :), intent(in) :: samples
        real, dimension(:, :), allocatable, intent(out) :: centroids
        real, dimension(n_features) :: mini, maxi
        integer :: current_centroid, current_feature
        real :: random_num

        allocate(centroids(n_clusters, n_features))

        do current_feature = 1, n_features
            mini(current_feature) = minval(samples(:, current_feature))
            maxi(current_feature) = maxval(samples(:, current_feature))
        end do

        do current_centroid = 1, n_clusters
            do current_feature = 1, n_features
                call random_number(random_num)
                centroids(current_centroid, current_feature) = &
                    mini(current_feature) + (maxi(current_feature) + 1 - mini(current_feature)) * random_num
            end do
        end do
    end subroutine initialize_centroids

    subroutine kmeans_serial(samples, centroids, assignments, n_iterations, n_samples, n_clusters, n_features)
        integer, intent(in) :: n_iterations, n_samples, n_clusters, n_features
        real, dimension(:, :), intent(in) :: samples
        real, dimension(:, :), allocatable, intent(out) :: centroids
        integer, dimension(:), intent(out) :: assignments
        integer :: current_iteration, current_sample, current_centroid, current_feature, counter
        real, dimension(n_features) :: sum_
        real :: optimum, distance

        ! Initialize centroid positions randomly
        call initialize_centroids(samples, centroids, n_samples, n_clusters, n_features)

        do current_iteration = 1, n_iterations
            ! Determine closest centroids for every sample
            do current_sample = 1, n_samples
                optimum = .0
                ! Do this for the first centroid since it will get picked anyway
                do current_feature = 1, n_features
                    optimum = optimum +&
                        (samples(current_sample, current_feature) - centroids(1, current_feature)) ** 2
                end do
                ! Check for the other centroids
                do current_centroid = 1, n_clusters
                    distance = .0
                    do current_feature = 1, n_features
                        ! Ignore square root since it is a strictly monotone rising function anyway
                        distance = distance +&
                            (samples(current_sample, current_feature) - centroids(current_centroid, current_feature)) ** 2
                    end do
                    if(distance < optimum) then
                        optimum = distance
                        assignments(current_sample) = current_centroid
                    end if
                end do
            end do

            ! Update the centroid positions
            do current_centroid = 1, n_clusters
                do current_feature = 1, n_features
                    sum_(current_feature) = .0
                end do
                counter = 0
                ! Sum up the positions of the assigned samples
                do current_sample = 1, n_samples
                    if(assignments(current_sample).eq.current_centroid) then
                        do current_feature = 1, n_features
                            sum_(current_feature) = sum_(current_feature) + samples(current_sample, current_feature)
                        end do
                        counter = counter + 1
                    end if
                end do
                ! Update the position of the centroid
                do current_feature = 1, n_features
                    if(counter.ne.0) then
                        centroids(current_centroid, current_feature) = sum_(current_feature) / counter
                    end if
                end do
            end do
        end do
    end subroutine kmeans_serial

    subroutine write_output(filename_output, samples, centroids, assignments, n_samples, n_clusters, n_features)
        character(len = *), intent(in) :: filename_output
        integer, intent(in) :: n_samples, n_clusters, n_features
        real, dimension(:, :), intent(in) :: samples, centroids
        integer, dimension(:), intent(in) :: assignments
        integer :: current_line

        ! Format: feature_1, 1x,..., 1x, feature_2, 1x, assigned_cluster, 1x, type

        open(unit = 11, file = filename_output)
        do current_line = 1, n_clusters
            write(11, *) centroids(current_line, :), current_line, 'c'
        end do
        do current_line = 1, n_samples
            write(11, *) samples(current_line, :), assignments(current_line), 's'
        end do
        close(11)
    end subroutine write_output
end module lib

program main
    use lib

    implicit None

    real :: start, finish
    integer :: n_iterations, n_samples, n_features, n_clusters
    character(len = :), allocatable :: filename_input
    real, dimension(:, :), allocatable :: samples, centroids
    integer, dimension(:), allocatable :: assignments

    ! Parse the arguments into the variables
    call get_arguments(n_iterations, filename_input, n_samples, n_clusters, n_features)
    ! Read the input file into the samples array and initialize the assignments array
    call read_samples(filename_input, samples, assignments, n_samples, n_features)

    print *, "Performing k-means clustering on ", filename_input, " with ",&
        n_iterations, " iteration(s)"

    call cpu_time(start)
    call kmeans_serial(samples, centroids, assignments, n_iterations, n_samples, n_clusters, n_features)
    call cpu_time(finish)
    call write_output("output/out.dat", samples, centroids, assignments, n_samples, n_clusters, n_features)
    print *, "Finished serial computation [time elapsed = ", finish-start, " sec.]"


end program main
