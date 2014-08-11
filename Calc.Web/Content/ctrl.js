(function (a) {
    'use strict';

    a.controller('Ctrl', ['$scope', function ($scope) {
        $scope.calc = function (e) {
            $scope.s = Big.eval(e).toFixed();
            var i = new BigInteger($scope.s);
            $scope.isPrime = i.isProbablePrime(20);
            $scope.e = '';
        };
    }]);
})(window.App);