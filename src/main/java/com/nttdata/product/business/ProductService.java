package com.nttdata.product.business;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.model.Product;
import reactor.core.publisher.Mono;

public interface ProductService {
    Mono<Product> getProduct(String type);

    Mono<Product> saveProduct(ProductRequest productRequest);

    Mono<Product> updateProduct(ProductRequest productRequest, String productId);

    Mono<Void> deleteProduct(String productId);

}
