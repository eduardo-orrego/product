package com.nttdata.product.repository;

import com.nttdata.product.model.Product;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ProductRepository {

    Mono<Product> findProduct(String productId);

    Flux<Product> findProducts(String type);

    Mono<Boolean> findExistsProduct(String productId);

    Mono<Boolean> findExistsProducts(String type);

    Mono<Product> saveProduct(Product product);

    Mono<Void> deleteProduct(String productId);
}
